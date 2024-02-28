(use spork/misc ./schema)

# Eleanor navigation works by the digesting points of the
# path and resetting the current base. Current base is initialy
# the datastructure provided to the function returned by the traverse.
# Every point on path then moves the base through the stucture,
# similarly to how core get-in works.
# The function then returns the latest base as its result.

(defn traverse
  ```
  Function that takes a path, which is variadic number
  of points. Point could be function of arrity one which
  is called with current `base`, and its return value is set
  as new base. For anything else value is used as a key.

  Returns function with arity of one. Its argument should be
  the datastructure it traverses
  ```
  [& path]
  (def compath
    (map
      (fn [p] (if (fn? p) p (fn getter [base] (get base p))))
      path))
  (fn traverse [ds]
    (var base ds)
    (each transfn compath
      (try
        (set base (transfn base))
        ([e f]
          (def prefix (string "Point " (describe transfn) " errored with: "))
          (if (dyn :debug)
            (debug/stacktrace f e prefix)
            (error (string prefix e))))))
    base))

(def => "traverse alias" traverse)

# Points are basic building blocks of the path
# for traverse. Selection here is not complete,
# and can be used as study material.
# Points must be functions, and can return function.
# All functions must have an arity of one.

(defn >map
  ```
  Returns function that maps `base` and `args`
  with the function `fun` and returns array for all
  members as new base.
  ```
  [fun & args]
  (fn map-fn [base]
    (map |(fun $ ;args) base)))

(defn >map-get
  ```
  Returns function that maps value under `key` from
  all members of the base.
  ```
  [key]
  (fn >map-get [base] (map |(in $ key) base)))

(def >: `>map-get alias` >map-get)

(defn >filter
  ```
  Returns function that filters all members of the base
  by the function `fun`.
  ```
  [fun]
  (fn >filter [base] (filter fun base)))

(def >Y `>filter alias` >filter)

(defn >check
  ```
  Returns function that checks if `which` members
  of the base conforms to `what` predicate.
  ```
  [which what]
  (fn >check [base] (which what base)))

(def >?? `check alias` >check)

(defn >check-all
  ```
  Returns function that checks if `which` for all `predicates` 
  returns true on base
  ```
  [which & predicates]
  (fn >check-all [base] (which |($ base) predicates)))

(defn >limit
  ```
  Returns a function, that limits the number of indexed
  base to `l` members. It retains the base type if possible.
  ```
  [l]
  (fn >limit [base]
    (if (> (length base) l)
      ((case (type base)
         :array array/slice
         :buffer buffer/slice
         :symbol symbol/slice
         :keyword keyword/slice
         slice) base 0 l)
      base)))

(def >n "Alias for limit" >limit)

(defn >collect
  ```
  Returns function that collects result of the `fun`
  call on `base` to `collected`. 
  `fun` is optional, if falsy whole base is collected.
  ```
  [collected &opt fun]
  (fn >collect [base]
    (array/push collected (if fun (fun base) base))
    base))

(def <- `collect alias` >collect)

(defn >merge
  ```
  Returns a function that merges all tables in base to optional `tab`,
  which defaults to `@{}`.
  ```
  [&opt tab]
  (default tab @{})
  (fn >merged [base] (merge tab ;base)))

(defn >merge-into
  "Returns function which merges `tab` into `base`."
  [tab]
  (fn >merge-into [base] (merge-into base tab)))

(defn >select-keys
  ```
  Returns function which selects `keys` from base 
  and returns new table just with them.
  ```
  [& keys]
  (fn >select [i] (select-keys i keys)))

(def >:: `>select-keys alias` >select-keys)

(defn >flatvals
  ```
  Flattens the values of each member of the base.
  ```
  [base]
  (def res @[])
  (loop [t :in base] (array/push res ;(values t)))
  res)

(defn >put
  "Returns a function, that changes base under the `key` to new `value`."
  [key value]
  (fn >put [base] (put base key value)))

(defn >update
  ```
  Changes base under the `key` to result of running `fun` on its value.
  ```
  [key fun]
  (fn >update [base] (update base key fun)))

(defmacro >clear
  "Clear `keys` of the table"
  [& keys]
  ~(=> ;,(map |(>put $ nil) keys)))

(defn >add
  ```
  Returns function that will push `value` into the array base.
  ```
  [value]
  (fn >add [base] (array/push base value)))

(defn >remove
  ```
  Returns function that will remove value from the array base at `index`.
  ```
  [index]
  (fn >remove [base]
    (array/remove base index)))

(defn >find-remove
  ```
  Returns function that will remove `value` from the array base.
  ```
  [value]
  (fn >remove-val [base]
    (array/remove base (find-index |(= value $) base))))

(defn >find-from-start
  ```
  Find first member of indexed `base` for which `pred` is truthy,
  starting from the start.
  ```
  [pred]
  (fn >find-from-start [base]
    (var i 0)
    (var res nil)
    (while (< i (length base))
      (def item (base i))
      (when (pred item) (set res item) (break))
      (++ i))
    res))

(defn >find-from-end
  ```
  Find first member of `base` for which `pred` is truthy
  starting from the end.
  ```
  [pred]
  (fn >find-from-end [base]
    (var i (dec (length base)))
    (var res nil)
    (while (>= i 0)
      (def item (base i))
      (when (pred item) (set res item) (break))
      (-- i))
    res))

(defn >from-start
  ```
  Returns `i`-th member of the indexed `base` counted from 
  the start of the base.
  ```
  [i]
  (fn >from-start [base] (in base i)))

(defn >from-end
  ```
  Returns i-th member of the indexed `base` counted from 
  the end of the base.
  ```
  [i]
  (fn >from-end [base]
    (def ni (- (length base) i 1))
    (if-not (neg? ni) (in base ni))))

(defn >partition-by
  "Returns function that partitions base on `fn`"
  [fn]
  (fn >paritition-by [base] (partition-by fn base)))

(defn >group-by
  "Returns function that groups base on `fn`"
  [fn]
  (fn >group-by [base] (group-by fn base)))

(defn >if
  ```
  Conditional navigation and transformation on predicate.

  * `pred` if it is a functions it receives the base, else it is evaluated
  * `tfnval` if it is a function it will receive base and result of the call 
    is set as the new base. Otherwise its value is set as the new base.
  * optional `ffnval` falsey branch of the conditional, same as `tfnval`
    but for the negative result of the `pred`.

  If predicates returns false, base is not changed.
  ```
  [pred tfnval &opt ffnval]
  (fn >if [base]
    (if (if (fn? pred) (pred base) pred)
      (if (function? tfnval) (tfnval base) tfnval)
      (if ffnval
        (if (function? ffnval) (ffnval base) ffnval)
        base))))

(defn >base
  "Sets `ds` as the new base."
  [ds]
  (fn >base [_] ds))

(def <-> "Alias to >base" >base)

(defn >assert
  "Asserts `pred` on the `base` and errors with `msg` if it fails."
  [pred &opt msg]
  (fn >assert [base] (assert (pred base) msg)))

(defn >map-keys
  "Maps all keys in table base with `mapfn`"
  [mapfn]
  (fn >map-keys [base] (map-keys mapfn base)))

(defn >map-vals
  "Maps all vals in table base with `mapfn`"
  [mapfn]
  (fn >map-vals [base] (map-vals mapfn base)))

(defn >trace-base
  "Tracev base"
  [base]
  (tracev base))
