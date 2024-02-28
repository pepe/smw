(use spork/misc)
(import spork/path)
(import spork/temple)

(use ./init ../events)

(define-watch ProjectDir
  "Event that creates project dir"
  [_ {:config {"name" n}} _]
  [(log "- " n) (mkdir n)])

(define-event Project
  "Event that renders project.janet file"
  {:update
   (fn [_ e]
     (def deps
       (string/join
         (seq [dep :in (get-in e [:config "dependencies"])]
           (string "\"" dep "\""))
         (string "\n" (string/repeat " " 17))))
     (put-in e [:config "dependencies"] deps))
   :watch
   (fn [_ {:config c} _]
     (def {"name" n} c)
     [(log "  - project.janet")
      (render n "project.janet" "project")])})

(define-watch License
  "Event that renders LICENSE file"
  [_ {:config config} _]
  (def {"license" l "name" n "author" a} config)
  [(log "  - LICENSE")
   (render n "LICENSE"
           (string "licenses/" (string/ascii-lower l))
           (merge config {"organization" a "year" ((os/date) :year)}))])

(define-watch Readme
  "Event that renders README.md file"
  [_ {:config {"name" n}} _]
  [(log "  - README.md")
   (render n "README.md" "README")])

(define-watch Init
  "Event that renders init.janet file"
  [_ {:config {"name" n "gen-init" gi}} _]
  (if gi
    [(prnmkdir n n)
     (log "    - init.janet")
     (render n (path/join n "init.janet") "init")]))

(define-watch Test
  "Event that renders test file"
  [_ {:config {"name" n}} _]
  [(log "  - test/")
   (mkdir (path/join n "test"))
   (render n (path/join "test" "init.janet")
           "test-init")])

(define-effect Git
  "Event that initalizes git repository"
  [_ {:config {"name" n "repo" r "origin" o}} _]
  (os/cd n)
  (try
    (do
      (os/execute ["git" "init"] :p)
      (if-let [u (or o r)] (os/execute ["git" "remote" "add" "origin" u] :p))
      (os/execute ["git" "add" "."] :p)
      (os/execute ["git" "commit" "-m" "Initial commit"] :p))
    ([e] (eprint "Failed to execute git. Repository not initilized")))
  (os/cd ".."))

(def default-config
  "Default configuration for the project"
  {"author" (author)
   "license" "mit"
   "dependencies" ["https://git.sr.ht/~pepe/gp"]
   "gen-init" true})

(define-event Start
  "Event that starts the generation"
  {:update
   (fn [_ e]
     (def c (merge default-config (e :config)))
     (put e :config c)
     (update-in e [:config "license"] string/ascii-upper))
   :watch
   (fn [_ {:config c} s]
     (cond->
       (flatten
         @[(log "Generating project with config:")
           (seq [[k v] :pairs c]
             (def tabs
               (string/repeat "\t" (math/ceil (/ (- 24 (inc (length k))) 8))))
             (log (string k) ":" tabs
                  (cond->> v (not (string? v)) (string/format "%j"))))
           (log "Tree: ")
           ProjectDir Project License Readme])
       (and (c "gen-init") (not (find |(= ($ :name) "Init") s)))
       (array/push Init)
       (and (c "test-init") (not (find |(= ($ :name) "Test") s)))
       (array/push Test)
       (and (c "git-init") (not (find |(= ($ :name) "Git") s)))
       (array/push Git)))})

(define-watch ConfigHelp
  "Event that prints the help"
  [&]
  (log
    ```
  Generate new project
  {"name" "Name of the project to generate the stub for"
   "author" "Author of the project default `(author)`"
   "license"  "License you want to use for this project default `mit`"
   "description" "Description of the project"
   "dependencies" "Tuple of space separated dependencies default [`gp`]"
   "declare-source" "Add source declaration to project.janet?"
   "repo" "Url of public git repository"
   "url" "Url with more info"
   "gen-init" "Generate init.janet for the project? default true"
   "git-init" "Create git repo and do first commit?"
   "origin" "Url with private origin git repo"
   "test-init" "Create tests"}
  ```))
