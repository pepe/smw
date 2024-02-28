(use jhydro spork/misc)

(use gp/codec)
(import ./server)
(use ./http)
(use ../data/schema)
(use ../utils)


(defn- key [buf]
  (setup-peg-grammar)
  (-?>> buf
        (peg/match '(* (thru "Sec-WebSocket-Key") ": " :cap-to-crlf))
        first))

(def- magic-string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defn- handshake [buf]
  (def key (key buf))
  (switching-protocols
    (->>
      (string key magic-string)
      picohash/sha1
      string
      base64/encode)))

(defn- unmask [mask masked-data]
  (do-def data (buffer/new (length masked-data))
          (loop [[i byte] :pairs masked-data]
            (buffer/push data (bxor byte (mask (% i 4)))))))

(defn- str->num [s nb]
  ((peg/match ~(uint ,nb) (string/reverse s)) 0))

(defn response
  ```
  Returns the string of websocket response with opcode `opc`
  and `message`.
  ```
  [opc message]
  (def dl (length message))
  (var b (buffer/push @"" (bor 2r10000000 opc)))
  (cond
    (> dl (math/pow 2 16))
    (buffer/push b 2r01111111 (int/to-bytes (int/u64 dl) :be))
    (> dl 125)
    (buffer/push b (string/from-bytes 2r01111110 (brshift dl 8) dl))
    (buffer/push b (string/from-bytes dl)))
  (buffer/push b message))

(defn text
  ```
  Returns a text message string.
  ```
  [message]
  (response 0x1 message))

(defn binary
  ```
  Returns a binary message string.
  ```
  [message]
  (response 0x2 message))

(defmacro supervisor
  ```
  Default supervisor which is used when you do not supply your own.
  It expects channel where to take the events and handling table
  for handling new connections.
  ```
  [chan handling & rules]
  (def additional-rules
    ~[,;rules
      [:error fiber]
      (let [err (fiber/last-value fiber)
            conn ((fiber/getenv fiber) :conn)]
        (unless (or (= err "Connection reset by peer")
                    (= err "stream is closed"))
          (eprint err)
          (debug/stacktrace fiber)
          (protect (:write conn (,text err)))
          (:close conn)))])
  ~(as-macro ,server/supervisor ,chan ,handling ,;additional-rules))

(defn- make-socket [connection handler]
  (table/setproto
    handler
    @{:write
      (fn write [self msg]
        (match
          (protect (:write connection msg))
          [true _] (protect (:flush connection))
          [false err]
          (do
            (ev/give-supervisor :close connection)
            (:closed self))))
      :check (fn check [&] true)
      :close
      (fn close [self msg]
        (:write self (response 0x8 msg))
        (ev/give-supervisor :close connection))}))

(defn on-connection
  ```
  A handler for the websockets connection.
  It is compatible with the `net/server` and can be used
  `handling` function argument.
  Its only argument is `handler` table, which should have
  at least `:connect`, `:read` and `:closed` methods.
  Methods are called when client connects, sends a message
  and closes the connection respectively.
  Optionaly you can provide `:check` method, which is called
  with the initial request and connection, and returns true
  if we can proceed with the connection, or false to decline it.
  You can also write to the connection, as it can be used for
  authentication for example.
  Handler is enriched with `:write` method for writting to
  the connection.
  Handler is enriched with `:close` method for closing
  the connection.
  ```
  [handler]

  (def buff-size 1024)
  (assert (??? table?
               {:connect function?
                :read function?
                :closed function?
                :check [some nil? function?]})
          "Handler is not valid")
  (fn on-connection [connection]
    (def handling (make-socket connection handler))
    (def req (:read connection buff-size))
    (when (and req
               (:check handling req connection)
               ((protect (:write handling (handshake req))) 0))
      (:flush connection)
      (:connect handling req)
      (def msg (buffer/new 125))
      (forever
        (def cbytes (:read connection 2))
        (unless cbytes
          (ev/give-supervisor :close connection)
          (break))
        (def [fb sb] cbytes)
        (def fin (band 2r10000000 fb))
        (def opc (band 2r00001111 fb))
        (def msk? (band 2r10000000 sb))
        (when (zero? msk?)
          (ev/give-supervisor :close connection)
          (break))
        (var pln (band 2r01111111 sb))
        (case pln
          126 (set pln (str->num (:read connection 2) 2))
          127 (set pln (str->num (:read connection 8) 8)))
        (if (pos? pln)
          (buffer/push msg (unmask (:read connection 4)
                                   (:read connection pln))))
        (case opc
          0x9 (:write handling (response 0xA msg))
          0x8 (do
                (:write connection (response 0x8 msg))
                (ev/give-supervisor :close connection)
                (break))
          (when (pos? fin)
            (:read handling opc msg)
            (buffer/clear msg)))))
    (:closed handling)
    (ev/give-supervisor :close connection)))

(defmacro server
  ```
  Convenience for spawning http server with default `supervisor`.
  
  It has one parameter `handler` with the object, that handles the requests.
  
    It also takes three optional parameters:
  - `host` hostname to bind to.
  - `port` port to bind to.
  - `rules` variadic rules' pairs for the supervisor pattern matching.
  ```
  [handler &opt host port & rules]
  ~(as-macro ,server/spawn ,supervisor (,on-connection ,handler)
             ,host ,port ,;rules))
