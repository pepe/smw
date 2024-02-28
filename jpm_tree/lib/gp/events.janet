(use spork/misc)
(use ./utils)

(defmacro producer
  ```
  Gives fiber to the Manager with tag `:producer`.
  This fiber will be running on `ev` and its supervisor
  will be set to the Manager flow channel.
  ```
  [& body]
  ~(coro
     (ev/give-supervisor :producer (coro ,;body)) nil))

(defmacro thread-producer
  ```
  Gives fiber to the Manager with tag `:producer-thread`.
  This fiber will be running on `ev` and its supervisor
  will be set to the Manager thread flow channel.
  ```
  [& body]
  ~(fiber/new (fn [] (ev/give-supervisor :thread-producer (coro ,;body)) nil)))

(defmacro produce
  ```
  Gives variadic number of Events to the supervisor with tag `:product`.
  These Events are immediately transacted my the Manager.
  ```
  [& events]
  ~(ev/give-supervisor :product [,;events]))

(def Event
  ```
  Event prototype used for creating events. It has three methods:
     * :update method receives Manager and State. You should mutate the state
       only in the update functions. Both arguments are mutable, but Manager
       mutation is very bad antipattern. Return value is ignored.
     * :watch method receives Manager, State and Stream. Return value is
       pushed into Tide for later fall. Tide fall throws when return value is
       not Event, Array of Events, Fiber. All three arguments are mutable, but Manager
       and State mutation is very bad antipattern.
     * :effect method receives Manager, State and Tide. Its main purpose is
       to trigger side-effects. All three arguments are mutable, but their
       mutation in effect function is very bad antipattern. Return value
       is ignored.

  And one member :name, which is usefull for debugging purposes.
  ```
  @{:name "anonymous"
    :update false
    :watch false
    :effect false})

(defn valid?
  ```
  Returns true if event is valid Event. Valid Event must be a table and
  have Event as a prototype.
  ```
  [event]
  (truthy?
    (and (table? event)
         (= Event (table/getproto event)))))

(defn make-event
  ```
  Creates new anonymous Event. Use it for dynamically created Events inside
  function.

  It has two parameters:
  * the `fns-table` with at least one of :update, :watch and :effect 
    methods. Its prototype will be set to the Event above.
  * the second optional parameter is Event `name`. Defaults to "anonymous".
  ```
  [fns-table &opt name]
  (default name (get fns-table :name "anonymous"))
  (make Event ;(kvs (merge fns-table {:name name}))))

(defmacro define-event
  ```
  Macro that defines new named Event. Use it for statically created Events.

  It has two or three parameters:
  * name: desired name for the Event. Preferably in PascalCase.
  * docstring: optional docstring for the Event
  * fns: table with methods. Same as for make function.
  ```
  [name & more]
  (def dftup (tuple make-event (last more) (string name)))
  (case (length more)
    2 (tuple 'def name (first more) dftup)
    1 (tuple 'def name dftup)))

(defn make-update
  ```
  Convenience function for creating event with only `:update` method
  with `fun`. Optional `name` works as in `make-event`.
  ```
  [fun &opt name]
  (make-event {:update fun} name))

(defn make-watch
  ```
  Convenience function for creating event with only `:watch` method
  with `fun`. Optional `name` works as in `make-event`.
  ```
  [fun &opt name]
  (make-event {:watch fun} name))

(defn make-effect
  ```
  Convenience function for creating event with only `:effect` method
  with `fun`. Optional `name` works as in `make-event`.
  ```
  [fun &opt name]
  (make-event {:effect fun} name))

(defn- define-*
  ```
  Utility function used by the define- macros.
  ```
  [typ name more]
  (var start 0)
  (def docstring
    (when (string? (more 0))
      (++ start)
      (more 0)))
  (def bindings (more start))
  (def body (slice more (inc start) -1))
  (def dfnt (tuple make-event {typ (tuple 'fn name bindings ;body)}
                   (string name)))
  (if docstring
    (tuple 'def name docstring dfnt)
    (tuple 'def name dfnt)))

(defmacro define-update
  ```
  Macro that defines event with only update event.
  It has two parameters:
  * name: desired name for the new event
  * more: if first member is a string, it is used as docstring.
  	Otherwise first member must be bindings tuple simillar to 
    fn bindings for the update fn. And rest is
    variadic body of the function
  ```
  [name & more]
  (define-* :update name more))

(defmacro define-watch
  ```
  Macro that defines event with only watch event.
  It has two parameters:
  * name: desired name for the new event
  * more: if first member is a string, it is used as docstring.
  	Otherwise first member must be bindings tuple simillar to 
    fn bindings for the update fn. And rest is
    variadic body of the function
  ```
  [name & more]
  (define-* :watch name more))

(defmacro define-effect
  ```
  Macro that defines event with only effect event.
  It has two parameters:
  * name: desired name for the new event
  * more: if first member is a string, it is used as docstring.
  	Otherwise first member must be bindings tuple simillar to 
    fn bindings for the update fn. And rest is
    variadic body of the function
  ```
  [name & more]
  (define-* :effect name more))

(defn- type-err [manager types evention v]
  (:on-error manager (string "Only " types " are " evention ". Got: " (type v))))

(defn- _process-stream
  ```
  Function that processes `manager`'s `_stream` and populates
  its `_flow` and `_thread-flow`. Do not use it on your own.
  ```
  [manager]
  (defer (put manager :processing nil)
    (put manager :processing true)
    (def stream (manager :_stream))
    (def chan (ev/chan 128))
    (var fibers 0)
    (defn transact-spliced [f]
      (match (fiber/last-value f)
        nil ()
        (event (valid? event)) (:transact manager event)
        (events (all valid? events)) (:transact manager ;events)
        bad (type-err manager "Event or Array of Events " "transactable" bad)))
    (defn inc-producers [] (update manager :_producers inc))
    (if-not (manager :_flow)
      (merge-into manager
                  {:_flow (ev/chan 128)
                   :_thread-flow (ev/thread-chan 128)}))
    (while (or (not (empty? stream)) (pos? fibers))
      (match (or (array/pop stream) (ev/take chan))
        (e (valid? e)) (:transact manager e)
        (fiber (fiber? fiber)
               (not (= (fiber/status fiber) :dead)))
        (do
          (ev/go fiber nil chan)
          (++ fibers))
        [:yield fiber]
        (do
          (transact-spliced fiber)
          (ev/go fiber nil chan))
        [:ok fiber]
        (do
          (transact-spliced fiber)
          (-- fibers))
        [:producer producer]
        (do
          (ev/go producer nil (manager :_flow))
          (inc-producers))
        [:thread-producer producer]
        (do
          (ev/thread producer nil :n (manager :_thread-flow))
          (inc-producers))))))

(defn transact
  ```
  Transact Event with Manager. This is the main way to process Events.

  The function has two parameters:

  * `manager`: Manager instance to which we are transacting
  * `events`: Events to transact. Variadic parameter.

  Throws on invalid Event.

  This function is called when you call `:transact` method on Manager.

  Returns Manager.
  ```
  [manager & events]
  (assert (all valid? events) (string "Only Events are transactable."))
  (def {:state state :_stream stream} manager)
  (each event events
    (if (event :update)
      (try (:update event state)
        ([_ errf] (:on-error manager [:update event errf]))))
    (if-let [watch (event :watch)]
      (cond
        (indexed? watch) (array/concat stream (reverse watch))
        (or (valid? watch) (fiber? watch)) (array/push stream watch)
        (match (try (watch event state stream)
                 ([_ errf] (:on-error manager [:watch event errf])))
          nil ()
          (events (indexed? events) (all valid? events))
          (array/concat stream (reverse events))
          (eventofib (or (valid? eventofib) (fiber? eventofib)))
          (array/push stream eventofib)
          (tableevent (valid? (make-event tableevent)))
          (array/push stream (make-event tableevent))
          bad
          (type-err manager "Event, Array of Events and Fiber" "watchable" bad))))
    (if (event :effect)
      (try (:effect event state stream)
        ([_ errf] (:on-error manager [:effect event errf])))))
  (if-not (manager :processing) (:_process-stream manager))
  manager)

(defn await
  ```
  Blocks until all Producers on the Manager supervisor channels finish.
  Returns array, where the first member is final state
  followed by all products from producers.

  This function is called when you call :await method on Manager.
  ```
  [manager]

  (def res @[])
  (defn dec-producers-add-res [val]
    (update manager :_producers dec)
    (array/push res val))
  (while (pos? (manager :_producers))
    (match (last (ev/select (manager :_thread-flow) (manager :_flow)))
      [:ok (producer (fiber? producer))]
      (dec-producers-add-res (fiber/last-value producer))
      [:ok val]
      (dec-producers-add-res val)
      [:yield producer]
      (array/push res (fiber/last-value producer))
      [:product events]
      (:transact manager ;(map |(if (valid? $) $ (make-event $)) events))))
  (array/insert res 0 (manager :state))
  res)

(def Manager
  ```
  Manager prototype. It has two public methods:

  * (:transact manager & events): transacts given Events.
  * (:await manager): waits for the manager to unzip all producers.
    Retuns the the array of the state and all producers results.
  ```
  @{:transact transact
    :await await
    :_stream @[]
    :_producers 0
    :_process-stream _process-stream})

(defn- default-on-error
  "Default on-error function for manager"
  [_ e]
  (match e
    (msg (string? e)) (error msg)
    (t (tuple? t))
    (eprintf "%p failed for %s with error: %s" (t 0) (get-in t [1 :name])
             (fiber/last-value (t 2)))))

(defn make-manager
  ```
  Factory function for creating new Manager with two optional parameters:

    * state: initial state for the Manager. Defaults to @{}.
    * on-error: error handler function. Err could be string, or tuple
      with the fn type (:update, :watch, :effect), event and err fiber.
      Defaults to `default-on-error`.
  Throws when state is not a table or on-error is not fn.
  Returns initialised Manager.
  ```
  [&opt state on-error]
  (default state @{})
  (default on-error default-on-error)
  (assert (table? state) "state must be table")
  (assert (function? on-error) "on-error must be function")

  (make
    Manager
    :state state
    :on-error on-error))
