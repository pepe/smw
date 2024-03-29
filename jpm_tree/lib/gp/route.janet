# @todo: make this dyn
(def chars
  "Characters considered part of the route"
  '(+ :w (set "-_.")))

(def sep "Separator character" "/")

(def pref "Param prefix character" ":")

(defn- named-capture [name & capture]
  ~(group (* (constant ,(keyword name)) ,;capture)))

(def- <-: named-capture)

(def grammar
  "PEG grammar to match routes with"
  (peg/compile
    {:sep sep
     :pref pref
     :path ~(some ,chars)
     :param '(* :pref :path)
     :capture-path '(<- :path)
     :main ~(some (* :sep
                     (+ (if :param ,(<-: :param :pref :capture-path))
                        (if :path ,(<-: :path :capture-path))
                        (if -1 ,(<-: :root '(constant -1))))))}))

(defn- compile-route
  "Compiles custom grammar for one route"
  [route]
  (-> (seq [[pt p] :in (peg/match grammar route)]
        (case pt
          :root (tuple '* sep p)
          :path (tuple '* sep p)
          :param (tuple '* sep
                        ~,(<-: p ~(<- (some ,chars))))))
      (array/insert 0 '*)
      (array/push -1)
      splice
      tuple
      peg/compile))

(defn- extract-args
  "Extracts arguments from peg match"
  [route-grammar uri]
  (when-let [p (peg/match route-grammar uri)]
    (table ;(flatten p))))

(defn compile-routes
  "Compiles PEG grammar for all routes"
  [routes]
  (def res @[])
  (loop [[route action] :pairs routes]
    (when (string? route) (array/push res [(compile-route route) action])))
  res)

(defn lookup
  "Looks up uri in routes and returns action and params for the matched route"
  [compiled-routes uri]
  (var matched [])
  (loop [[grammar action] :in compiled-routes :while (empty? matched)]
    (when-let [args (extract-args grammar uri)] (set matched [action args])))
  matched)

(defn router
  "Creates a simple router from routes"
  [routes]
  (def compiled-routes (compile-routes routes))
  (fn [path] (lookup compiled-routes path)))

(defn resolver
  "Creates a simple route compiler from routes"
  [routes]
  (def inverted-routes (invert routes))
  (fn [action &opt params]
    (def template (assert (get inverted-routes action) (string/format "Route %s does not exist" action)))
    (if params
      (let [params-grammar (seq [[k v] :pairs params] ~(/ (<- (* ":" ,(string k))) ,(string v)))]
        (first (peg/match ~(% (any (+ ,;params-grammar (<- 1)))) template)))
      template)))
