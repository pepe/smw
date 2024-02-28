(import ./base :prefix "" :export true)

# HTTP utils
(defn <li/>
  "Wraps item in li"
  [& clss]
  (def class (string "f-row " (string/join clss " ")))
  |[:li {:class class} ;$])

(defn <ul/>
  "Wraps item in ul"
  [&opt cls]
  |[:ul {:class cls} $])

(defn input-datetime
  "Converts dt to string representation for input value"
  [dt]
  (string (dt/format-date dt) "T" (dt/format-time dt)))

(defn check-session
  ```
  Checks if user cookie is in the sessions. If it is found cookie "user"
  is merged into `req` with which `next-middleware` is called. If the session
  is not found it refirects to `/auth`.
  ```
  [next-middleware]
  (http/cookies
    (fn check-session [req]
      (define :view)
      (def sk (get-in req [:headers "Cookie" "session"]))
      (if-let [ck (and sk (get-in view [:sessions sk]))]
        (next-middleware (put req :session ck))
        (http/see-other "/auth")))))

(defn layout-or-hx
  "Wraps response struct in app layout when not hx"
  [next-middleware & args]
  (def default-response {:status 200 :body "" :headers @{}})
  (def default-headers (http/content-type ".html"))
  (fn layout-or-hx [req]
    (def resp (merge default-response (next-middleware req)))
    (update resp :headers merge default-headers)
    (http/http
      (if (get-in req [:headers "HX-Request"])
        resp
        (merge resp
               {:body (http/page app (merge (table ;args)
                                            {:content (resp :body)}))})))))

(defn notify
  "Sends message by sse"
  [type &opt msg]
  (make-effect
    (fn [_ {:sse-chans chans} _]
      (if (next chans) (each chan chans (ev/give chan [type msg]))))
    "notify"))

(defn flash
  "Construct notify event for flashing message to all"
  [& msg]
  (notify :flash (string ;msg)))

(define-update Dirty
  "Marks store as dirty"
  [_ state]
  (put state :dirty true))

(define-update Clean
  "Marks store as clean"
  [_ state]
  (put state :dirty false))

(define-event Flush
  "Flushes the store"
  {:watch (fn [&] [(log "Flushing store") Clean])
   :effect (fn [_ {:store store} _] (:flush store))})

(defn register-chan
  "Event that registers new channel."
  [chan]
  (make-update
    (fn [_ e] ((=> :sse-chans (>add chan)) e))
    "register-chan"))

(defn deregister-chan
  "Event that deregisters channel."
  [chan]
  (make-event
    {:update
     (fn [_ e] ((=> :sse-chans (>find-remove chan)) e))
     :effect (fn [&] (:close chan))}
    "deregister-chan"))

(defn heart-beat
  "Periodic heart beat event"
  [& event-pairs]
  (assert (even? (length event-pairs)))
  (def rules (partition 2 event-pairs))
  (make-watch
    (fn [_ state _]
      (producer
        (var heart 0)
        (forever
          (++ heart)
          (each [events pred] rules
            (if (pred heart state) (produce ;events)))
          (ev/sleep 1))))
    "heart-beat"))

(define-event PrepareStore
  "Prepares store in the state."
  {:update (fn [_ state] (put state :store (make Store :image (state :image))))
   :watch (fn [_ {:image image :log log?} _]
            (if log? (log "Initializing store image named " image)))
   :effect (fn [_ {:store s} _] (:init s))})

(define-effect Stop
  "Stop the server, flush store and exits"
  [_ {:store store} _]
  (ev/sleep 0.1) (:flush store) (os/exit))

(define-watch Netrepl
  "Start the netrepl"
  [_ state _]
  (def {:netrepl nr} state)
  (if nr
    (producer
      (netrepl/server ;(server/host-port nr) (put (curenv) :state state)))))

(define-watch HTTP
  "Creates producer with running HTTP server."
  [_ {:http http :view view :resolve resolve :key key :log log?
      :routes routes :public public :static static} _]
  (producer
    (def chan (ev/chan 128))
    (server/start chan ;(server/host-port http))
    (def drive-fn
      (http/on-connection
        (http/parser
          (cond-> routes
                  static (put :not-found (http/static public))
                  true http/drive
                  log? event-journal))))
    (http/supervisor
      chan drive-fn
      [:product events] (produce ;events)
      [:error fiber]
      (let [err (fiber/last-value fiber)
            conn ((fiber/getenv fiber) :conn)]
        (unless (http/closed-err? err)
          (eprint "HTTP Supervisor: " err)
          (when (dyn :debug) (debug/stacktrace fiber))
          (protect
            (:write conn
                    (http/internal-server-error
                      (string "Internal Server Error: " err)))))
        (:close conn)))))

(define-watch RPC
  "Creates producer with running RPC server."
  [_ {:rpc url :store store :env env :psk psk} _]
  (producer (def [host port] (string/split ":" url))
            (def chan (ev/chan))
            (server/start chan host port)
            (rpc/supervisor
              chan
              (rpc/on-connection
                @{:psk psk
                  :stop (fn [&] (produce (log "Servers are going down") Stop) :dying)})
              [:product events] (produce ;events)
              [:error fiber]
              (do
                (def err (fiber/last-value fiber))
                (def conn ((fiber/getenv fiber) :conn))
                (produce (log "RPC Supervisor: " err))
                (when (dyn :debug) (produce (stacktrace fiber)))
                (:close conn)))))

(define-watch Present
  "Creates event that prints present message for the server"
  [_ {:http http :rpc rpc :log log?} _]
  (if log?
    [(logf "Starting HTTP server on %s, port %s" ;(server/host-port http))
     (logf "Starting RPC server on %s, port %s" ;(server/host-port rpc))]))

(defn on-error
  "Manages errors for events' manager. Transacts detail logging."
  [manager err]
  (:transact
    manager
    ;(match err
       (msg (string? msg)) (log msg)
       ([at event f] (keyword? at) (valid? event) (fiber? f))
       (cond-> @[(log (string at " failed for " (event :name)
                              " with error: " (fiber/last-value f)))]
               (dyn :debug) (array/push (stacktrace f)))
       (error "Unexpected error type"))))
