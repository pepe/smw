(import spork/sh :export true)
(import ./base :prefix "" :export true)

(defn- files-with-mod [dir]
  (tabseq [[i f] :pairs (sh/list-all-files dir)] f (os/stat f :modified)))

(defn monitor
  ```
  Creates event, that monitors directory `dir` and produces result of calling 
  `fun` with name of the file that changes.
  ```
  [dir fun]
  (make-watch
    (fn [&]
      (thread-producer
        (var files (files-with-mod dir))
        (var file nil)
        (forever
          (ev/sleep 1)
          (def current-files (files-with-mod dir))
          (eachk f current-files
            (unless (= (files f) (current-files f))
              (print "\nFile " f " modified")
              (set file f))
            (unless (files f)
              (print "\nFile " f " created")
              (set file f))
            (when file
              (produce (fun file))
              (set file nil)
              (ev/sleep 0.001)
              (set files (files-with-mod dir)))))))))

(defn mdz->html
  "Changes mdz to html externsion"
  [file &opt prefix]
  (default prefix "")
  (->> file
       (string/replace "content" prefix)
       (string/replace "mdz" "html")))

(defn process-css
  "Process css"
  [e]
  (def {:static s
        :css css
        :files {:css fcss}} e)
  (->>
    fcss
    (map |(string/replace (path/join s css) css $))
    (filter |(string/has-suffix? ".css" $))
    sort))

(defn fix-nl
  "Fix end line to unix"
  [fc]
  (string/replace-all "\r\n" "\n" fc))

(defn layout
  "Renders app template with provided arguments."
  [args]
  (http/page app args))

(define-watch Present
  "Prints present message"
  [_ {:site-title t} _]
  (log t " construction starts"))

(define-update SetDev
  "Sets dev in the state"
  [_ e] (put e :dev true))

(defn save-content
  "Saves content to the file"
  [file content]
  (make-effect
    (fn [_ {:content c :public p} _]
      (def nf
        (->> file
             (string/replace c p)
             (string/replace "mdz" "html")))
      (def dir (path/dirname nf))
      (if (not (os/stat dir)) (os/mkdir dir))
      (spit nf content)
      (print "Rendered " file " to " nf))
    (string "save-content-" file)))

(defn render-content-file
  "Renders mdz file"
  [file]
  (make-watch
    (fn [_ e _]
      (try
        (do
          (put module/cache file nil)
          (let [{:site-title st :dev dev :static s :logos logos :template t} e
                m (mdz/markup (slurp (string "./" file))
                              (require "spork/mdz" :prefix "") file)
                mt (get-in m [:front-matter :template])
                rt (or (and mt (temple/compile (slurp (string "." mt ".temple")))) t)
                args (merge (m :front-matter)
                            {:current-file file
                             :content (hg/html (m :markup-dom))
                             :site-title st :css (process-css e)
                             :logo logos :dev dev})]
            (save-content file (rt ;(kvs args)))))
        ([err fib]
          [(log "Error: " err " when rendering file: " file)
           (stacktrace fib)])))
    (string "render-content-" file)))

(define-watch RenderContent
  "Renders all content files"
  [_ {:files {:content cf}} _]
  (seq [f :in cf] (render-content-file f)))

(defn copy-file
  "Copies file from static to public"
  [file]
  (make-effect
    (fn [_ {:public p :static s} _]
      (def nf
        (string/replace s p file))
      (sh/copy-file file nf)
      (print "Copied " file " to " nf))
    (string "copy-file-" file)))

(define-watch CopyFiles
  "Copy all files and images"
  [_ {:files {:css cf :img im}} _]
  (seq [f :in (array/concat cf im)] (copy-file f)))

(defn save-files
  "Save all files to state"
  [dir files]
  (make-update
    (fn [_ e] (put-in e [:files dir] files))
    (string "save-files-" dir)))

(defn list-all-ext
  ```
  List all files in the `dir`. If provided one or more `exts` filenames
  are filtered with it.
  ```
  [dir & exts]
  (cond->> (sh/list-all-files dir)
           (not (empty? exts))
           (filter |(some (fn [ext] (string/has-suffix? ext $)) exts))))

(define-watch ListContent
  "Lists all content files"
  [_ {:content cd} _]
  (save-files :content (list-all-ext cd "mdz")))

(define-watch ListCss
  "Lists all css files"
  [_ {:static s :css cd} _]
  (save-files :css (list-all-ext (path/join s cd) "css" "woff2")))

(define-watch ListImg
  "Lists all img files"
  [_ {:static s :img cd} _]
  (save-files :img (list-all-ext (path/join s cd))))

(define-update MakeTemplate
  "Compiles template and save it to state"
  [_ e]
  (put e :template
       (temple/compile
         (slurp (path/join (e :templates) "/index.temple")))))

(define-watch CopyLogo
  "Copies logo image"
  [_ {:static s :logo logo} _]
  (copy-file (path/join s logo)))

(define-update SlurpLogo
  "Slurps logo"
  [_ e]
  (def {:public p :logo l} e)
  (put e :logos (slurp (path/join p l))))

(define-watch Rendering
  "All rendering events"
  [&]
  [ListCss
   ListImg
   CopyFiles
   CopyLogo
   SlurpLogo
   MakeTemplate
   ListContent
   RenderContent
   (log "Rendered everything")])

(defn /dashboard
  "Handler for the dashboard page"
  [state]
  (fn [&]
    (def {:site-title st :css css :files fs} state)
    (layout @{:title "Dashboard"
              :site-title st
              :css (process-css state)
              :content (http/page dashboard @{:files fs})})))

(defn /render
  "Handler for render action"
  [{:body {"file" file}}]
  (if (= file "all")
    (do
      (produce RenderContent)
      (http/see-other "/"))
    (do
      (produce (render-content-file file))
      (http/see-other (mdz->html file)))))

(defn /edit
  "Handler for the edit page"
  [state]
  (fn [{:query-params {"file" file}}]
    (def {:site-title st :css css :files fs} state)
    (def fc
      (if (= :file (os/stat file :mode))
        (slurp file)
        (http/page new-content {:author (state :author)
                                :templates (state :templates)})))
    (layout @{:title (string "Editing " file)
              :site-title st
              :css (process-css state)
              :content (http/page edit @{:file-content fc
                                         :file-name file})})))

(defn /save
  "Handler for the save action"
  [cd]
  (fn [{:body {"file-name" fnm "file-content" fc}}]
    (def san-fnm
      (let [trfn (string/trim fnm)]
        (cond->
          (not (string/has-suffix? ".mdz" trfn)) (string ".mdz")
          (not (string/has-prefix? "content" trfn)) (path/join "content"))))
    (spit san-fnm (fix-nl (string/trim fc))) # TODO add event
    (produce (render-content-file san-fnm) ListContent)
    (http/response 303 "" {"Location" (mdz->html san-fnm) "Content-Length" 0})))

(defn /upload
  "Handler for the upload action"
  [state]
  (fn [req]
    (layout @{:title (string "Upload new image file")
              :site-title (state :site-title)
              :css (process-css state)
              :content (http/page upload @{})})))

(defn /process
  "Handler process uploaded image"
  [static img]
  (fn [{:body body}]
    (spit (path/join static (body "path")) (gett body "content" :content))
    (produce ListImg)
    (http/see-other "/__dashboard")))

(defn handler
  "Main http application handler"
  [state]
  (-> {"/__dashboard"
       {"" (-> (/dashboard state)
               (http/guard-methods "GET")
               http/html-success)
        "/render" (-> /render
                      (http/guard-methods "POST")
                      http/urlencoded)
        "/edit" (-> (/edit state)
                    http/query-params
                    (http/guard-methods "GET")
                    http/html-success)
        "/save" (-> (/save (state :content))
                    (http/guard-methods "POST")
                    http/urlencoded)
        "/upload" (-> (/upload state) (http/guard-methods "GET") http/html-success)
        "/process" (-> (/process (state :static)
                                 (state :img))
                       (http/guard-methods "POST")
                       http/multipart)}
       :not-found (http/static "public")}
      http/drive event-journal http/parser))

(define-event HTTP
  "Starts http server"
  {:watch
   (fn [_ state _]
     (def {:http {:host host :port port}} state)
     (producer
       (def chan (ev/chan 128))
       (server/start chan host port)
       (http/supervisor
         chan (http/on-connection (handler state))
         [:product events] (produce ;events)
         [:error fiber]
         (let [err (fiber/last-value fiber)
               conn ((fiber/getenv fiber) :conn)]
           (unless (http/closed-err? err)
             (eprint "HTTP Supervisor: " err)
             (debug/stacktrace fiber)
             (protect
               (:write conn
                       (http/internal-server-error
                         (string "Internal Server Error: " err)))))
           (:close conn)))))
   :effect
   (fn [_ {:http {:host host :port port}} _]
     (print "HTTP Present on " host ":" port))})

(def env-init
  "Events per environment"
  {"dev" [HTTP Rendering SetDev Present
          (monitor "static" |(case $ "logo.svg" CopyLogo (copy-file $)))
          (monitor "content" render-content-file)]
   "prod" [Rendering Present]})
