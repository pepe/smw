(use spork/misc spork/zip jhydro)

(use ../data/schema)
(import ./server)
###
### hydrpc.janet
###
### Crypto RPC server and client tailored to Janet.
###
### Parts blatantly stolen from janet-lang/spork/rpc

### Limitations: ????
###
### Currently calls are resolved in the order that they are sent
### on the connection - in other words, a single RPC server must resolve
### remote calls sequentially. This means it is recommended to make multiple
### connections for separate transactions.

(use spork/msg)

(def ctx "Dynamic context for hydro" (dyn :neil-ctx "neilneil"))

(defn- make-encoder [msg-id session-pair]
  (fn encoder [msg]
    (-> msg
        marshal
        compress
        (secretbox/encrypt msg-id ctx (session-pair :tx)))))

(defn- make-decoder [msg-id session-pair]
  (fn decoder [msg]
    (-> msg
        (secretbox/decrypt msg-id ctx (session-pair :rx))
        decompress
        unmarshal)))

(defmacro supervisor
  ```
  Default supervisor.
  Param chan is the supervising channel of the server,
  handling is the handling object.
  ```
  [chan handling & rules]
  (def additional-rules
    ~[,;rules
      [:error fiber]
      (do
        (def err (fiber/last-value fiber))
        (def conn ((fiber/getenv fiber) :conn))
        (eprint err)
        (:close conn))])
  ~(as-macro ,server/supervisor ,chan ,handling ,;additional-rules))

(defn on-connection
  ```
  Create a handler for the RPC server. It must take a dictionary of handler
  with methods that clients can call. Under the :psk must be the preshared key
  for the jhydro handler.
  This function can be used by the `net/server`.
  ```
  [handler]

  (assert ((??? table? present?) handler) "Handler is not valid")
  (def psk (handler :psk))
  (put handler :psk nil)
  (def keys-msg (freeze (keys handler)))
  (def {:public-key pk :secret-key sk} (kx/keygen))
  (def known-peers @{})

  (fn on-connection [connection]
    (defn handshake []
      (def hrecv (make-recv connection identity))
      (def hsend (make-send connection identity))
      (var packet1 (hrecv))
      (if-let [[peer-pk _] (known-peers packet1)]
        (do
          (set packet1 (hrecv))
          (def packet2 (buffer/new 48))
          (def ret [(kx/kk2 packet2 packet1 peer-pk pk sk) peer-pk])
          (hsend packet2)
          ret)
        (do
          (def packet2 (buffer/new 96))
          (def state (kx/xx2 packet2 packet1 psk pk sk))
          (hsend packet2)
          (def packet3 (hrecv))
          (def peer-pk (buffer/new 32))
          [(kx/xx4 state packet3 psk peer-pk) peer-pk])))

    (match (protect (handshake))
      [false err]
      (do
        (ev/give-supervisor :close connection)
        (break))
      [true [session-pair peer-pk]]
      (do
        (var msg-id 0)
        (def recv (make-recv connection (make-decoder msg-id session-pair)))
        (def send (make-send connection (make-encoder msg-id session-pair)))
        (def peer-name (recv))
        (put known-peers peer-name [peer-pk (os/time)])
        (send keys-msg)
        (forever
          (match (protect (recv))
            [true (msg (not (nil? msg)))]
            (let [[fnname args] msg
                  f (handler fnname)]
              (++ msg-id)
              (if-not f
                (send [false (string "no function " fnname " supported")])
                (send (protect (f handler ;args)))))
            (do
              (ev/give-supervisor :close connection)
              (break))))))))

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

(def Client
  ```
  Prototype for the RPC client.
  TODO: document
  ```
  @{:open (fn open [self]
            (set (self :stream) (net/connect (self :host) (self :port)))
            (merge-into self (kx/keygen))
            (match (protect (:handshake self))
              [true _] (:setup-connection self)
              [false err] (error err)))
    :close (fn close [self]
             (:close (self :stream))
             (set (self :stream) nil)
             self)
    :reopen
    (fn reopen [self]
      (set (self :stream) (net/connect (self :host) (self :port)))
      (def hrecv (make-recv (self :stream) string))
      (def hsend (make-send (self :stream) string))
      (hsend (self :name))
      (def packet1 (buffer/new 48))
      (def state (kx/kk1 packet1 (self :peer-pk)
                         (self :public-key) (self :secret-key)))
      (hsend packet1)
      (def packet2 (hrecv))
      (set (self :session-pair)
           (kx/kk3 state packet2 (self :public-key) (self :secret-key)))
      (:setup-connection self))
    :handshake
    (fn handshake [self]
      (def {:public-key pk :secret-key sk} self)
      (def hrecv (make-recv (self :stream) string))
      (def hsend (make-send (self :stream) string))
      (def packet1 (buffer/new 48))
      (def state (kx/xx1 packet1 (self :psk)))
      (hsend packet1)
      (def packet2 (hrecv))
      (def packet3 (buffer/new 64))
      (def peer-pk (buffer/new 32))
      (set (self :session-pair)
           (kx/xx3 state packet3 packet2 (self :psk) pk sk peer-pk))
      (set (self :peer-pk) peer-pk)
      (hsend packet3))
    :setup-connection
    (fn setup-connection [self]
      (var msg-id 0)
      (def recv
        (make-recv (self :stream) (make-decoder msg-id (self :session-pair))))
      (def send
        (make-send (self :stream) (make-encoder msg-id (self :session-pair))))
      (send (self :name))
      (def fnames (recv))
      (each f fnames
        (set (self (keyword f))
             (fn rpc-function [_ & args]
               (send [f args])
               (++ msg-id)
               (let [[ok x] (recv)]
                 (if ok x (error x))))))
      self)})

(defn client
  ```
  Create an RPC client. Returns a table of async functions
  that can be used to make remote calls. This prototype contains
  a `:close` and `:reopen` methods that can be used to close and
  reopen the connection respectively.
  ```
  [&opt host port name psk]

  (def client
    (make Client
          :host host
          :port port
          :psk psk
          :name name))
  (:open client))
