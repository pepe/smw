(use ./environment)
(use ./parser)

# Events
(define-event InitStore
  "Initializes store"
  {:update
   (fn [_ state] (put state :store (make Store :image (state :image))))
   :effect (fn [_ state _] (:init (state :store)))})

(define-watch ^refresh-viewer
  "Sends sse with the content of the slide."
  [_ {:view view} _]
  (notify :viewer))

(define-event ^refresh-view
  "Refreshes view after change"
  {:update
   (fn [_ {:view view}]
     (let [chapter (view :chapter)
           slide (view :slide)]
       (merge-into view
                   {:slide-content ((=> :presentation :chapters chapter :slides slide) view)
                    :disabled-next-slide (not ((=> :presentation :chapters chapter :slides (inc slide)) view))
                    :disabled-previous-slide (zero? slide)
                    :disabled-next-chapter (not ((=> :presentation :chapters (inc chapter)) view))
                    :disabled-previous-chapter (zero? chapter)})))
   :watch ^refresh-viewer})

(define-event InitView
  "Initializes handlers' view"
  {:update (fn [_ state]
             (put state :view
                  @{:presentation (:load (state :store) :presentation)
                    :slide 0 :chapter 0}))
   :watch ^refresh-view
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

(define-event ^start
  "Sets positions to start"
  {:update
   (fn [_ {:view view}]
     (merge-into view @{:chapter 0 :slide 0}))
   :watch ^refresh-view})

(define-event ^next-slide
  "Moves slide forward"
  {:update (fn [_ {:view view}] (update view :slide inc))
   :watch ^refresh-view})

(define-event ^previous-slide
  "Moves slide backward"
  {:update (fn [_ {:view view}] (update view :slide dec))
   :watch ^refresh-view})

(define-event ^next-chapter
  "Moves chapter forward"
  {:update (fn [_ {:view view}] (update view :chapter inc) (put view :slide 0))
   :watch ^refresh-view})

(define-event ^previous-chapter
  "Moves chapter backward"
  {:update (fn [_ {:view view}] (update view :chapter dec) (put view :slide 0))
   :watch ^refresh-view})

# Transformations
(defn presentation->preview
  "Creates presentation preview in htmlgen"
  [presentation]
  [:main
   [:h1 (presentation :title)]
   [:section
    [:h1 "Chapters"]
    (seq [chapter :in (presentation :chapters)]
      [:p
       [:h3 (chapter :title)]
       [:span (length (chapter :slides)) " slides"]])]
   [:nav {:class "f-row"} [:a {:href "/edit"} "edit"] [:a {:href "/start"} "start"]]])

# Handlers
(defn /index
  "Index handler."
  [&]
  (define :view)
  (http/page app {:content
                  (if-let [presentation (view :presentation)]
                    (hg/html (presentation->preview presentation))
                    (http/page form))}))

(defn /save
  "Save handler."
  [req]
  (produce (^save (get-in req [:body "presentation"])))
  (http/see-other "/"))

(defn /edit
  "Show edit form."
  [&]
  (define :view)
  (http/page app {:content (http/page form {:content (get-in view [:presentation :content])
                                            :message "Edit presentation!"})}))

(defn /start
  "Start presentation"
  [&]
  (produce ^start)
  (http/see-other "/presentation"))

(defn /presentation
  "Show presentation"
  [&]
  (define :view)
  (http/page presentation (tracev view)))

(defn /next-slide
  "Moves presentation one slide forward"
  [&]
  (define :view)
  (produce ^next-slide)
  (http/response 200 "OK" @{"HX-Trigger" "refresh"}))

(defn /previous-slide
  "Moves presentation one slide forward"
  [&]
  (define :view)
  (produce ^previous-slide)
  (http/response 200 "OK" @{"HX-Trigger" "refresh"}))

(defn /next-chapter
  "Moves presentation one chapter forward"
  [&]
  (define :view)
  (produce ^next-chapter)
  (http/response 200 "OK" @{"HX-Trigger" "refresh"}))

(defn /previous-chapter
  "Moves presentation one chapter forward"
  [&]
  (define :view)
  (produce ^previous-chapter)
  (http/response 200 "OK" @{"HX-Trigger" "refresh"}))

(defn /slide
  "Returns current slide partial"
  [&]
  (define :view)
  (hg/html (view :slide-content)))

(defn /navigation
  "Returns current navigation partial"
  [&]
  (define :view)
  (http/page navigation view))

(defn /viewer
  "Show the current slide"
  [&]
  (define :view)
  (http/page viewer view))

(defn /viewer.sse
  "SSE entry point for the viewer"
  [&]
  (let [chan (ev/chan 128)]
    (produce (register-chan chan))
    (http/stream
     (forever
       (try
         (let [[type msg] (ev/take chan)] (http/event type msg))
         ([e]
          (ev/give-supervisor :close (dyn :conn))
          (break))))
     (produce (deregister-chan chan)))))

# Configuration
(def routes
  "Application routes"
  @{"/" (http/dispatch @{"GET" (http/html-get /index)
                         "POST" (http/urlencoded /save)})
    "/edit" (http/html-get /edit)
    "/start" /start
    "/presentation" (http/html-get /presentation)
    "/next-slide" /next-slide
    "/previous-slide" /previous-slide
    "/next-chapter" /next-chapter
    "/previous-chapter" /previous-chapter
    "/slide" (http/html-get /slide)
    "/navigation" (http/html-get /navigation)
    "/viewer" (http/html-get /viewer)
    "/viewer.sse" /viewer.sse})

(def config
  "Configuration"
  @{:image :store
    :http "localhost:7777"
    :routes routes
    :static true
    :public "public"
    :log true
    :sse-chans @[]})

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
