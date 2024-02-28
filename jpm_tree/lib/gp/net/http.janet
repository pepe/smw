(use spork/http spork/misc)
(import spork/json)
(import spork/temple)
(import spork/path)

(import ./server)
(import ./uri)
(import ../route)
(import ../utils)
(temple/add-loader)

# Reading part
(def buff-size "Default buffer size" 16384)

(utils/setup-peg-grammar)

(def- content-length-grammar
  (peg/compile
    ~{:cl "Content-Length: "
      :main (* (thru :cl) (/ '(to :crlf) ,scan-number)
               (thru (repeat 2 :crlf))
               (/ '(to -1) ,(fn content-length [b] (if b (length b) 0))))}))

(defn ensure-length
  ```
  Ensures that request is read whole in the most cases.
  Uses multiple passes according to the type of the request.
  It clears the request if it cannot be read in 16 pasess of
  16384 bytes.
  ```
  [connection req]
  (var reading 32)
  (var last-index 0)
  (while (pos? reading)
    (cond
      (def cls (string/find "Content-Length:" req))
      (do
        (var len-diff (- ;(peg/match content-length-grammar req cls)))
        (if (pos? len-diff)
          (:chunk connection len-diff req))
        (set reading 0))
      (not (string/find "\r\n\r\n" req))
      (do
        (:read connection buff-size req)
        (if (string/find "\r\n\r\n" req last-index)
          (set reading 1)
          (do
            (set last-index (length req))
            (if (one? reading) (buffer/clear req))
            (-- reading))))
      (set reading 0))))

(defn on-connection
  ```
  It takes `handler` with the user function,
  that will handle connections.
  Returns function for handling incomming connection,
  suitable for a default supervisor handling argument.
  Returned function reads the request and ensure its length.
  If it cannot be read in `ensure-length` it will write
  Entity too large response to the connection and closes it.
  ```
  [handler]
  (assert (function? handler) "Handler is not valid")
  (fn on-connection [connection]
    (forever
      (def req (buffer/new buff-size))
      (:read connection buff-size req)
      (when (empty? req)
        (ev/give-supervisor :close connection)
        (break))
      (ensure-length connection req)
      (when (empty? req)
        (:write connection
                "HTTP/1.1 413 Request Entity Too Large\r\nContent-Length: 24\r\nContent-Type: text/plain\r\n\r\nRequest Entity Too Large")
        (ev/give-supervisor :close connection)
        (break))
      # todo chunked response
      (def res (handler req))
      (if (bytes? res)
        (ev/write connection res)
        (do
          (res connection)
          (ev/give-supervisor :close connection))))))

# Managing part
(defmacro supervisor
  ```
  It takes `chan` as the supervising channel of the server
  and `handling` as the handling function.
  This supervisor is used by default if you do not
  provide your own to `server`.
  ```
  [chan handling & rules]
  (def additional-rules
    ~[,;rules
      [:error fiber]
      (let [err (fiber/last-value fiber)]
        (unless (or (= err "Connection reset by peer")
                    (= err "stream is closed"))
          (debug/stacktrace fiber err)
          (def conn ((fiber/getenv fiber) :conn))
          (protect
            (:write conn
                    "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 21\r\nContent-Type: text/plain\r\n\r\nInternal Server Error"))
          (:close conn)))])
  ~(as-macro ,server/supervisor ,chan ,handling ,;additional-rules))

(defmacro server
  ```
  Convenience for spawning http server with default `supervisor`.
  
  It has one parameter `handler` with the function, that handles the requests.
  
    It also takes three optional parameters:
  - `host` hostname to bind to.
  - `port` port to bind to.
  - `rules` variadic rules' pairs for the supervisor pattern matching.
  ```
  [handler &opt host port & rules]
  ~(as-macro ,server/spawn ,supervisor (,on-connection ,handler)
             ,host ,port ,;rules))

# Utils
(defn coerce-fn
  "Coerce any non fn to the fn returning it."
  [action]
  (if (function? action) action (fn coerced-action [r] action)))

(defn parse-request
  "Parses the http request."
  [reqs]
  (utils/setup-peg-grammar)
  (defn- caprl [m u q v]
    {:method m
     :uri u
     :query-string q
     :http-version v})
  (defn- caph [n c] {n c})
  (defn- colhs [& hs] {:headers (merge ;hs)})
  (defn- capb [b] {:body b})
  (defn- colr [& xs] (merge ;xs))
  (def- request-grammar
    (peg/compile
      ~{:sp " "
        :http "HTTP/"
        :cap-to-sp (* '(to :sp) :sp)
        :request (/ (* :cap-to-sp '(to (+ "?" :sp))
                       (any "?") :cap-to-sp :http :cap-to-crlf) ,caprl)
        :header (/ (* (not :crlf) '(to ":") ": " :cap-to-crlf) ,caph)
        :headers (/ (* (some :header) :crlf) ,colhs)
        :body (/ '(any (to -1)) ,capb)
        :main (/ (* :request :headers :body) ,colr)}))
  ((peg/match request-grammar reqs) 0))

(defn url-path
  "Matches the path from the first line of `req`."
  [req]
  ((peg/match '(* "GET " '(to " HTTP")) req) 0))

(defn closed-err?
  "Checks if the error is one of the closing ones."
  [err]
  (or (= err "Connection reset by peer")
      (= err "stream is closed")
      (= err "Broken pipe")))

(def mime-types
  "Mime types lookup table from file extension"
  {"*" "*/*"
   ".html" "text/html"
   ".htm" "text/html"
   ".txt" "text/plain"
   ".css" "text/css"
   ".js" "application/javascript"
   ".json" "application/json"
   ".xml" "text/xml"
   ".svg" "image/svg+xml"
   ".jpg" "image/jpeg"
   ".jpeg" "image/jpeg"
   ".gif" "image/gif"
   ".png" "image/png"
   ".wasm" "application/wasm"
   ".ico" "image/x-icon"
   ".csv" "text/csv"
   ".sse" "text/event-stream"})

(def mimes-charsets "Mime charsets that defaults to UTF-8"
  [".html" ".htm" ".json" ".xml" ".svg" ".sse"])

(defn http
  ```
  Turns a response dictionary into an http response string.
  It only uses contents under `:status`, `:body` and `headers`
  keys in the dictionary. They defaults to 200, "" and {}
  respectively.
  ```
  [{:status status :body body :headers headers}]
  (default status 200)
  (default body "")
  (default headers {})
  (def fh @"")
  (def dflth
    (if (< 300 status 399)
      {"Content-Length" 0}
      {"Content-Length" (string (length body))
       "Content-Type" (mime-types ".txt")}))
  (xprinf fh "HTTP/1.1 %d %s\r\n"
          status (get status-messages status "Unknown Status Code"))
  (loop [[n c] :pairs (merge dflth headers)]
    (if (dictionary? c)
      (loop [[k v] :pairs c]
        (xprinf fh "%s: %s\r\n"
                (string n)
                (string/format "%s=%s" k v)))
      (xprinf fh "%s: %s\r\n"
              (string n)
              (if (indexed? c)
                (string/join c ",")
                (string c)))))
  (xprin fh "\r\n")
  (if (and body (not (empty? body)))
    (xprin fh (string body)))
  fh)

(defn chunked-http
  ```
  Turns a response dictionary into an http response string.
  It only uses contents under `:status`, `:body` and `headers`
  keys in the dictionary. `status` defaults to 200 and 
  `headers` defaults to {}. Body must be a fiber that yields
	chunks. Transfer-Encoding is set to chunked.
  ```
  [{:status status :body body :headers headers}]
  (default status 200)
  (default headers {})
  (assert body "Body fiber must be present")
  (def dflth
    (if (< 300 status 399)
      (error "Staus code cannot be 3XX")
      {"Content-Type" (mime-types ".txt")
       "Transfer-Encoding" "chunked"}))
  (fn chunked-http [conn]
    (defn conn-write [f & values]
      (ev/write conn (string/format f ;values)))
    (conn-write
      "HTTP/1.1 %d %s\r\n"
      status (get status-messages status "Unknown Status Code"))
    (loop [[n c] :pairs (merge dflth headers)]
      (if (dictionary? c)
        (loop [[k v] :pairs c]
          (conn-write "%s: %s\r\n"
                      (string n)
                      (string/format "%s=%s" k v)))
        (conn-write "%s: %s\r\n"
                    (string n)
                    (if (indexed? c)
                      (string/join c ",")
                      (string c)))))
    (conn-write "\r\n")
    (each chunk body
      (conn-write "%x\r\n%s\r\n" (length chunk) chunk))
    (conn-write "0\r\n\r\n")))

(defmacro event
  "Send type of data to SSE."
  [type data]
  ~(do
     (if-not (= :data ,type)
       (:write conn (string "event: " ,type "\n")))
     (:write conn (string "data: " ,data "\n\n"))))

(defmacro stream
  "Creates new SSE stream"
  [& body]
  (def stream-resp
    "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream; charset=UTF-8\r\n\r\n")
  ~(fn stream [conn]
     (:write conn ,stream-resp)
     ,;body))

(defn response
  ```
  Creates response struct from http `code`, `body`
  and optional `headers`.
  ```
  [code body &opt headers]
  (default headers @{})
  (http
    {:status code
     :headers headers
     :body body}))

(defn success
  ```
  Return success response with optional `body` and `headers`.
  ```
  [&opt body headers]
  (default body (status-messages 200))
  (response 200 body headers))

(defn no-content
  ```
  Return no content response with optional `body` and `headers`.
  ```
  [&opt body headers]
  (default body (status-messages 200))
  (response 204 body headers))

(defn created
  ```
  Return created response with optional `body` and `headers`.
  ```
  [&opt body headers]
  (default body (status-messages 201))
  (response 201 body headers))

(defn bad-request
  "Returns bad request response with optional `body` and `headers`."
  [&opt body headers]
  (default body (status-messages 400))
  (response 400 body headers))

(defn not-authorized
  "Returns not autorized response with optional `body` and `headers`."
  [&opt body headers]
  (default body (status-messages 401))
  (response 401 body headers))

(defn not-found
  "Returns not found response with optional `body` and `headers`."
  [&opt body headers]
  (default body (status-messages 404))
  (response 404 body headers))

(defn not-supported
  ```
  Returns not supported media type response
  with optional `body` and `headers`.
  ```
  [&opt body headers]
  (default body (status-messages 415))
  (response 415 body headers))

(defn method-not-allowed
  "Returns not allowed method type response with optional `body` and `headers`."
  [&opt body headers]
  (default body (status-messages 405))
  (response 405 body headers))

(defn internal-server-error
  "Returns internal server error response with optional `body` and `headers`."
  [&opt body headers]
  (default body (status-messages 500))
  (response 500 body headers))

(defn not-implemented
  ```
  Returns not implemented method type response
  with optional `body` and `headers`.
  ```
  [&opt body headers]
  (default body (status-messages 501))
  (response 501 body headers))

(defn found
  "Returns found response with `location`."
  [location]
  (response 302 "" {"Location" location "Content-Length" 0}))

(defn see-other
  "Returns see other response with `location`."
  [location]
  (response 303 "" {"Location" location "Content-Length" 0}))

(defn switching-protocols
  "Returns switching protocols response with `key`"
  [key]
  (response 101 "" {"Upgrade" "websocket"
                    "Connection" "Upgrade"
                    "Sec-WebSocket-Accept" key}))

(defn content-type
  ```
  Returns Content-Type header for given `mime-type`.
  Optional `charset` defaults to UTF-8 where applicable.
  ```
  [mime-type &opt charset]
  (var mt (mime-types mime-type))
  (when (find |(= mime-type $) mimes-charsets)
    (default charset "UTF-8")
    (set mt (string mt "; charset=" charset)))
  {"Content-Type" mt})

(defn cookie
  ```
  Returns header for setting cookie with `value` under `key`.
  When optional `existing-cookie` header provided, it updates
  it with the new value.
  ```
  [key value &opt existing-cookie]
  (def [sk sv] [(string key) (string value)])
  (if existing-cookie
    (update existing-cookie "Set-Cookie" put sk sv)
    @{"Set-Cookie" @{sk sv}}))

(defn ->json
  "Encodes `data-structure` into json."
  [data-structure]
  (json/encode data-structure))

(defmacro page
  `Macro that converts temple template into html string`
  [name & body]
  (import* (string (dyn :templates "/templates") "/" name))
  (def iname
    (if-let [si (string/find "/" name)]
      (slice name (inc si))
      name))
  (def b (if (next body) body [{}]))
  (with-syms [args buf]
    ~(do
       (def ,buf @"")
       (def ,args ,;b)
       (with-dyns [:out ,buf]
         (,(symbol iname "/render-dict") ,args))
       (freeze ,buf))))

(defn page*
  "Function that converts temple template into html string"
  [name args]
  (def rend
    (-> (dyn :templates "/templates")
        (string "/" name)
        require
        (get-in ['render-dict :value])))
  (def buf @"")
  (with-dyns [:out buf] (rend args))
  (freeze buf))

(defn tag
  ```
  Returns string with html tag `name` with enclosed `content`.
  Optional `attrs` must be table of attributes.
  ```
  [name content &opt attrs]
  (default attrs {})
  (assert (dictionary? attrs))
  (def attrss @"")
  (loop [[k v] :pairs attrs]
    (buffer/push-string attrss " " k `="` (string v) `"`))
  (string `<` name attrss `>` content `</` name `>`))

(defn etag
  ```
  Returns string with html empty tag `name`.
  Optional `attrs` must be table of attributes.
  ```
  [name &opt attrs]
  (tag name "" attrs))

(defn ptag
  ```
  Prints html tag `name` with enclosed `content`.
  Optional `attrs` must be table of attributes.
  ```
  [name content &opt attrs]
  (prin (tag name content attrs)))

(defn petag
  ```
  Prints html empty tag `name`.
  Optional `attrs` must be table of attributes.
  ```
  [name &opt attrs]
  (prin (etag name attrs)))

(defn parser
  "Parses the http request into request table"
  [next-middleware]
  (fn parser [req]
    (next-middleware (parse-request req))))

# Middleware
(defn journal
  ```
  Middleware that logs the request.
  ```
  [next-middleware &opt printer]
  (default printer
    |(eprintf "%s %s %s in %s, %s"
              ($ :head) ($ :method) ($ :fulluri) ($ :elapsed) ($ :reqs)))
  (def headg ''(thru (* " " :d+)))
  (fn journal [req]
    (def {:uri uri
          :method method
          :query-string qs} req)
    (def start (os/clock))
    (def resp (next-middleware req))
    (def elapsed (- (os/clock) start))
    (def metrics @{:method method
                   :elapsed (utils/precise-time elapsed)
                   :reqs (string/format "%.3freqs/s" (/ 1 elapsed))})
    (when (bytes? resp)
      (put metrics :head ((peg/match headg resp) 0))
      (put metrics :fulluri (if (and qs (not (empty? qs)))
                              (string uri "?" qs) uri))
      (printer metrics))
    resp))

(defn drive
  ```
  Creates a router middleware.

  The first argument should be the table of routes you want to define.
  Keys are the bytes sequence with path, value is the function to call
  or table. In case of table key is used as prefix for all keys in value table.
  The subtable is then flattened with prefixes.

  If you define route :not-found that will be matched if no defined one does.
  ```
  [routes]
  (def comproutes @{})
  (loop [[k v] :pairs routes]
    (if (dictionary? v)
      (loop [[sk sv] :pairs v]
        (put comproutes (string k sk) (coerce-fn sv)))
      (put comproutes k (coerce-fn v))))
  (def ruter (route/router comproutes))
  (def not-found-action
    (coerce-fn (or (routes :not-found) (not-found))))
  (fn drive [req]
    (def [action params] (ruter (req :uri)))
    (if action
      (action (put req :params params))
      (not-found-action req))))

(defn query-params
  "Parses query string into janet struct under :query-params key.
   Keys are keywordized"
  [next-middleware]
  (fn query-params [req]
    (def query-string (req :query-string))
    (if (empty? query-string)
      (next-middleware req)
      (do
        (-?>> query-string
              uri/parse-query
              (map-vals uri/unescape)
              (put req :query-params))
        (if (nil? (req :query-params))
          (bad-request "Query params have invalid format")
          (next-middleware req))))))

(defn urlencoded
  ```
  Creates middleware function, that parses urlencoded body
  into janet table with parameters.
  ```
  [next-middleware]
  (defn decode [body]
    (->> body
         string/trim
         (string/replace-all "+" "%20")
         uri/parse-query
         (map-vals
           |(->> $
                 uri/unescape
                 (case $ "false" false "true" true $)))))
  (fn urlencode [req]
    (if (= (gett req :headers "Content-Type")
           "application/x-www-form-urlencoded")
      (update req :body decode))
    (next-middleware req)))

(defn multipart
  ```
  Creates middleware function, that parses multipart encoded body
  into janet table with parameters.
  ```
  [next-middleware]
  (defn capf [h c] {h c})
  (defn capfn [n c ct d]
    {n {:filename c
        :content-type ct
        :content d}})
  (defn capm [& fs] (merge ;fs))
  (fn multipart [req]
    (if-let [[bndr]
             (peg/match '(* "multipart/form-data; boundary=" '(to -1))
                        (get-in req [:headers "Content-Type"]))]
      (update
        req :body
        |(->>
           $
           (peg/match
             ~{:crlf "\r\n"
               :boundary (* "--" ,bndr)
               :be "--"
               :boundaryn (* :crlf :boundary (? :be) :crlf)
               :quote "\""
               :cd "Content-Disposition: form-data; name="
               :fn (* "; filename=" :quote '(to :quote) :quote :crlf
                      "Content-Type: " '(to :crlf))
               :header (* :cd :quote '(to :quote) :quote)
               :content (* '(to :boundaryn) :boundaryn)
               :field (/ (* :header (repeat 2 :crlf) :content) ,capf)
               :file (/ (* :header :fn (repeat 2 :crlf) :content) ,capfn)
               :main (* :boundary :crlf (/ (some (+ :field :file)) ,capm))})
           first)))
    (next-middleware req)))

(defn cookies
  ```
  Creates middleware function, that parses the cookies from the headers.
  ```
  [next-middleware]
  (def grammar
    '{:end (+ -1 "; ")
      :sep "="
      :pair (* '(to :sep) :sep '(to :end) :end)
      :main (some :pair)})
  (fn cookies [req]
    (if-let [ck (get-in req [:headers "Cookie"])]
      (put-in req [:headers "Cookie"] (table ;(peg/match grammar ck))))
    (next-middleware req)))

(defn json->body
  ```
  Creates middleware that parses json in body
  into Janet struct under :body key
  ```
  [next-middleware]
  (fn json->body [req]
    (let [b (req :body)]
      (if (empty? b)
        (next-middleware req)
        (->> b
             json/decode
             (put req :body)
             next-middleware)))))

(defn guard-methods
  "Middleware for quarding only some http methods"
  [next-middleware & methods]
  (fn guard-methods [req]
    (def method (req :method))
    (if (or (= method "OPTIONS") (some |(= method $) methods))
      (next-middleware req)
      (method-not-allowed
        (string/format
          "Method '%s' is not supported. Please use %s"
          method (string/join methods " or "))))))

(defn guard-mime
  "Guards mime content type"
  [next-middleware mime]
  (def all-mime (mime-types "*"))
  (def req-mime (mime-types mime))
  (fn guard-mime [req]
    (def accept (get-in req [:headers "Accept"] "*/*"))
    (if (or (string/find all-mime accept) (string/find req-mime accept))
      (next-middleware req)
      (not-supported
        (string/format
          "Media '%s' is not supported, please use '%s' or '%s'"
          accept req-mime all-mime)))))

(defn dispatch
  ```
  Dispatches based on HTTP methods. Configuration is in
  the table where keys must be HTTP methods in allcaps.
  ```
  [config]
  (fn dispatch [req]
    (def method (req :method))
    (if-let [action (config method)]
      ((coerce-fn action) req)
      (not-implemented
        (string/format
          "Method %s is not implemented, please use %s"
          method (string/join (keys config) " or "))))))

(defn static
  ```
  Serves static files in a given directory.
  ```
  [directory &opt default-index]
  (default default-index "index.html")
  (fn static [req]
    (def uri (req :uri))
    (def path
      (if (string/has-suffix? "/" uri)
        (path/join directory uri default-index)
        (path/join directory uri)))
    (if (= :file (os/stat path :mode))
      (response 200 (slurp path) (content-type (path/ext path)))
      (not-found))))

(defn typed
  ```
  Similar to dispatch, but works on mime types. Configuration is the
  table, where keys are mime extensions and values are functions to run.
  ```
  [config]
  (fn typed [req]
    (def accept (get-in req [:headers "Accept"] "*"))
    (def mime ((invert mime-types) accept))
    (if-let [action (config mime)]
      ((coerce-fn action) req)
      (not-supported
        (string/format
          "Media '%s' is not supported, please use one of %s."
          mime (string/join
                 (map (fn format-mime [m]
                        (string "'" (mime-types m) "'"))
                      (keys config)) ", "))))))

(defn html-success
  ```
  Create middleware which takes response and returns it with
  html-mime and success status
  ```
  [next-middleware]
  (fn html-success [req]
    (success (next-middleware req) (content-type ".html"))))

(defn style
  "Renders css style tag for `ds`"
  [ds]
  (do
    (def res @"")
    (loop [[e d] :in ds]
      (buffer/push res e " " "{")
      (loop [[n v] :pairs d]
        (buffer/push res n ": " v ";"))
      (buffer/push res "}"))
    res))

(defn html-success-resp
  "Wraps `resp` with success status and html mime"
  [resp &opt headers]
  (default headers @{})
  (merge-into headers (content-type ".html"))
  (success resp headers))

(defn html-get
  ```
  Guards the get http method, check the session and wraps `next-middleware`
  with the `html-success`.
  ```
  [next-middleware]
  (-> next-middleware
      (guard-methods "GET")
      html-success))

(defn keywordize-body
  "Make keys in body keywords"
  [next-middleware]
  (fn [req]
    (next-middleware
      (update req :body |(map-keys keyword $)))))

(defn urlenc-post
  ```
  Wraps the `next-middleware` in urlencode, guards the post method,
  flushes, rerenders and checks the session.
  ```
  [next-middleware]
  (-> next-middleware
      keywordize-body
      urlencoded
      (guard-methods "POST")))

(defn urlenc-put
  ```
  Wraps the `next-middleware` in urlencode, guards the put method,
  flushes, rerenders and checks the session.
  ```
  [next-middleware]
  (-> next-middleware
      keywordize-body
      urlencoded
      (guard-methods "PUT")))

(defn- process-attrs
  [attrs]
  (cond
    (empty? attrs) {}
    (all bytes? attrs) {:class (string/join attrs " ")}
    (dictionary? (attrs 0)) (freeze (attrs 0))))

(defmacro make-wrap
  "Creates function `<el/>` for wrapping"
  [el]
  (def name (symbol "<" el "/>"))
  (def attrs (gensym))
  ~(defn ,name
     ,(string "Wraps item in " el)
     [& ,attrs]
     (fn [& items]
       [,(keyword el) (,process-attrs ,attrs) ;items])))
