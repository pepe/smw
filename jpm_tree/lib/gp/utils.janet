(import spork/sh)
(import spork/misc)
# TODO test
(defmacro fprotect
  ```
  Similar to core library `protect`. Evaluate expressions `body`, while
  capturing any errors. Evaluates to a tuple of two elements. The first
  element is true if successful, false if an error. The second is the return
  value or the fiber that errored respectively. 
  Use it, when you want to get the stacktrace of the error.
  ```
  [& body]
  (with-syms [fib res err?]
    ~(let [,fib (,fiber/new (fn [] ,;body) :ie)
           ,res (,resume ,fib)
           ,err? (,= :error (,fiber/status ,fib))]
       [(,not ,err?) (if ,err? ,fib ,res)])))

(defmacro first-capture
  "Returns first match in string `s` by the peg `p`"
  [p s]
  ~(first (peg/match ,p ,s)))

(defn union
  "Returns the union of the the members of the sets."
  [& sets]
  (def head (first sets))
  (def ss (array ;sets))
  (while (not= 1 (length ss))
    (let [aset (array/pop ss)]
      (each i aset
        (if-not (find-index |(= i $) head) (array/push head i)))))
  (first ss))

(defn intersect
  "Returns the intersection of the the members of the sets."
  [& sets]
  (def ss (array ;sets))
  (while (not= 1 (length ss))
    (let [head (first ss)
          aset (array/pop ss)]
      (put ss 0 (filter (fn [i] (find-index |(deep= i $) aset)) head))))
  (first ss))

(def peg-grammar
  "Custom peg grammar with crlf and to end."
  (merge (dyn :peg-grammar)
         ~{:crlf "\r\n"
           :cap-to-crlf (* '(to :crlf) :crlf)
           :toe '(to -1)
           :boundaries (+ :s (set ",.?!_-/|\\"))
           :split (any (+ :boundaries '(some :a)))}))

(defn setup-peg-grammar
  "Merges `peg-grammar` into `:peg-grammar` `dyn`"
  []
  (setdyn :peg-grammar peg-grammar))

(defn named-capture
  ```
  Creates group where the first member is keyword `name`
  and other members are `captures`.
  ```
  [name & captures]
  ~(group (* (constant ,(keyword name)) ,;captures)))

(def <-: "Alias for named-capture." named-capture)

(defmacro one-of
  ```
  Takes value `v` and variadic number of values in `ds`,
  and returns the `v` if it is present in the `ds`.
  ```
  [v & ds]
  ~(or ,;(seq [i :in ds] ~(= ,v ,i))))

(defmacro define
  "Define symbol from dyn under `key`."
  [key]
  (assert (keyword? key))
  ~(def ,(symbol key) (dyn ,key)))

(defn all-project-files
  ```
  Returns all code files in the project as array of strings, 
  or if `modified` is truthy as table where filenames are keys
  and their last modification time as value.
  ```
  [&opt modified]
  (def list
    (filter
      |(peg/match '(to (* (+ "janet" "temple") -1)) $)
      (array/concat (sh/list-all-files "./")
                    ["project.janet"])))
  (if modified
    (tabseq [[i f] :pairs list] f (os/stat f :modified))
    list))

(defn watch
  "Spawns commands, watch all project files and respawns on changes."
  [& cmds]
  (var ift (all-project-files true))
  (var s (os/spawn cmds :p))
  (var restart false)
  (forever
    (def cft (all-project-files true))
    (eachk f cft
      (unless (= (ift f) (cft f))
        (print "\nFile " f " modified")
        (set restart true))
      (unless (ift f)
        (print "\nFile " f " created")
        (set restart true))
      (when restart
        (os/proc-kill s)
        (print "Restarting")
        (set s (os/spawn cmds :p))
        (set ift (all-project-files true))
        (set restart false)))
    (ev/sleep 1)))

(def jpm
  "On windows you have to add .bat"
  (misc/cond-> "jpm" (= (os/which) :windows) (string ".bat")))

(defn precise-time
  ```
  Returns precise time `t` with s, ms, us, ns precision
  as a string.
  ```
  [t]
  (string/format
    ;(cond
       (zero? t) ["0s"]
       (>= t 1) ["%.3fs" t]
       (>= t 1e-3) ["%.3fms" (* t 1e3)]
       (>= t 1e-6) ["%.3fus" (* t 1e6)]
       (>= t 1e-9) ["%.3fns" (* t 1e9)])))
