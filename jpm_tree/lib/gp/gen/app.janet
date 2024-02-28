(use spork/misc)
(import spork/path)
(use ../events ./init)
(import ./project)

(define-watch Dirs
  "Event that creates all directories"
  [_ {:config {"name" n}} _]
  (seq [d :in ["app/" "public/" "templates/"]]
    (prnmkdir n d)))

(define-watch Templates
  "Event that renders all template files"
  [_ {:config {"name" n}} _]
  [(log "  - templates/app.template")
   (render n "templates/app.temple" "app/app")])

(define-watch Code
  "Event that renders all code files"
  [_ {:config {"name" n}} _]
  [(log "  - app/init.janet")
   (render n "app/init.janet" "app/init")
   (log "  - app/environment.janet")
   (render n "app/environment.janet" "app/environment")])

(define-watch Instruction
  "Event that logs further instructions"
  [_ {:config {"name" n "bin" b}} _]
  [(log "App " n " generated! Congratulations!")
   (log "Next steps:")
   (log "> cd " n)
   (log "> jpm -l deps")
   (log "> jpm -l janet app/init.janet")])

(define-event Start
  "Event that starts creation of the app"
  {:update
   (fn [_ state]
     (def {:config config} state)
     (put state :config
          (merge
            {"author" (author)
             "http" "localhost:7777"
             "gen-init" false}
            config)))
   :watch
   (fn [_ {:config config} _]
     [;(cond-> @[project/Start Dirs Code Templates]
               (config "gen-test") (array/push project/Test)
               (config "git-init") (array/push project/Git))
      Instruction])})

(define-watch ConfigHelp [&]
  [(log
     ```
    Generate new app
    {"site-title" "Title of the new site"
     "bin" "name-of-binary"
     "http" {:host "0.0.0.0" :port 7777}}

    This recipe also runs the project recipe:

    ```)
   project/ConfigHelp])
