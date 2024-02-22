(use ./environment)

(defn /index
  "Index page."
  [req]
  (http/page app {:content "Hello World!"}))

(def routes
  "Application routes"
  @{"/" (http/html-get /index)})

(def config
  "Configuration"
  @{:http "localhost:7777"
    :routes routes
    :log true})

(defn main
  ```
	Main entry into smw.

	Initializes manager, transacts HTTP and awaits it.
  ```
  [_ &opt runtime]
  (default runtime @{})
  (assert (table? runtime))
  (-> config
      (merge-into runtime)
      (make-manager on-error)
      (:transact HTTP (log "See My Work is running on " (config :http)))
      :await))
