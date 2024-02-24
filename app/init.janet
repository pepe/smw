(use ./environment)
(use ./parser)

# Events
(define-event InitStore
  "Initializes store"
  {:update
   (fn [_ state] (put state :store (make Store :image (state :image))))
   :effect (fn [_ state _] (:init (state :store)))})

(define-event InitView
  "Initializes handlers' view"
  {:update (fn [_ state]
             (put state :view
                  @{:presentation (:load (state :store) :presentation)}))
   :effect (fn [_ state _] (setdyn *view* (state :view)))})

(defn ^save
  "Creates event that parses and saves presentation to store and view"
  [presentation-content]
  (make-event
    {:update
     (fn [_ {:store store :view view}]
       (def presentation (parse-presentation presentation-content))
       (:save store presentation :presentation)
       (put view :presentation presentation))
     :effect (fn [_ {:store store} _] (:flush store))}
    "save"))

# Handlers
(defn /index
  "Index handler."
  [req]
  (define :view)
  (http/page app {:content
                  (if-let [presentation (view :presentation)]
                    (hg/html [:main [:h1 (presentation :title)]
                              [:section
                               [:h1 "Chapters"]
                               (seq [chapter :in (presentation :chapters)]
                                 [:p (chapter :title)])]])
                    (http/page form))}))

(defn /save
  "Save handler."
  [req]
  (produce (^save (gett req :body "presentation")))
  (http/see-other "/"))

# Configuration
(def routes
  "Application routes"
  @{"/" (http/dispatch @{"GET" (http/html-get /index)
                         "POST" (http/urlencoded /save)})})

(def config
  "Configuration"
  @{:image :store
    :http "localhost:7777"
    :routes routes
    :static true
    :public "public"
    :log true})

# Main entry point
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
      (:transact InitStore InitView)
      (:transact HTTP (log "See My Work is running on " (config :http)))
      :await))
