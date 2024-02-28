(use spork/misc)
(import spork/path)
(use ../events ./init)
(import ./project)

(define-watch Dirs
  "Event that creates all directories"
  [_ {:config {"name" n}} _]
  (seq [d :in ["static" "static/css" "content"
               "public" "public/css" "templates"]]
    (prnmkdir n d)))

(define-watch Templates
  "Event that renders all template files"
  [_ {:config {"name" n}} _]
  (flatten
    (seq [template :in ["app" "dashboard" "edit"
                        "foot" "head" "index" "new-content" "upload"]
          :let [tf (string "templates/" template ".temple")
                ttf (path/join "static-web/templates" template)]]
      [(log "    - " template)
       (render n tf ttf)])))

(define-watch Code
  "Event that renders all code files"
  [_ {:config {"name" n "bin" b}} _]
  (flatten
    [(prnmkdir n "app")
     (flatten
       (seq [template :in ["init" "environment"]
             :let [tf (path/join "app" (string template ".janet"))
                   tt (path/join "static-web" template)]]
         [(log "    - " template)
          (render n tf tt)]))
     (log "  - " b)
     (render n b "static-web/bin")
     (executable (path/join n b))]))

(define-watch Content
  "Event that renders all content files"
  [_ {:config {"name" n}} _]
  [(log "  - content/index.mdz")
   (render n "content/index.mdz" "static-web/content/index")
   (log "  - static/logo.svg")
   (touch (path/join n "static/logo.svg"))])

(define-watch Instruction
  "Event that logs further instructions"
  [_ {:config {"name" n "bin" b}} _]
  [(log "Static " n " site generated! Congratulations!")
   (log "Next steps:")
   (log "> cd " n)
   (log "> jpm deps")
   (log "> ./" b)])

(define-event Start
  "Event that starts creation of the static web"
  {:update
   (fn [_ state]
     (def {:config config} state)
     (put state :git-init (config "git-init"))
     (put state :config
          (merge
            {"site-title" (or (config "site-title")
                              (string "Site for " (config "name")))
             "author" (author)
             "http" {:host "0.0.0.0" :port 7777}
             "bin" (config "name")}
            config
            {"git-init" false
             "gen-test" false})))
   :watch
   (fn [_ state _]
     [;(cond-> @[project/Start Dirs Templates Code Content]
               (state :git-init) (array/push project/Git))
      Instruction])})

(define-watch ConfigHelp [&]
  [(log
     ```
    Generate new static site
    {"site-title" "Title of the new site"
     "bin" "name-of-binary"
     "http" {:host "0.0.0.0" :port 7777}}

    This recipe also runs the project recipe:

    ```)
   project/ConfigHelp])
