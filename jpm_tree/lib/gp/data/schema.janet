# Simple module for validating and analysing
# data structures in Janet.
# It has at the moment two main modes of function, which
# coresponds to the two functions in this module:
# - validator
# takes schema and returns function which takes datastructure
# as an argument. If the datastructure conform to the schema
# it is returned unchanged, if not false is returned.
# - analyst
# takes schema and returns function which takes datastructure
# as an argument. If datastructure conforms to the schema
# empty parts of the schema are returned. If not, offending
# parts are returned according to schema with the predicates
# that were not met.

(def schema
  ```
  Schema is variadic argument for module functions, where members
  are predicates for the type of the data:
  - functions (string?, struct? etc.) with which the whole datastructure
    is tested.
  - a dictionary, where keys could be one of:
    * function, which is used to extract the items from data to validate
    * any other value, which is used as key to get from data
  - and value is function, which is used to validate
  ```
  ())

(defn fn?
  "Returns `true` if `what` is function or cfunction."
  [what]
  (if-not (function? what) (cfunction? what) true))

(defn validator
  ```
  Creates function which can be used for validating the data.
  It has one argument schema. See `(doc schema)`
  The function returns the data structure unchanged when it is valid or
  false.
  ```
  [& schema]
  (if (empty? schema)
    (fn truth [_] true)
    (fn validator [data]
      (var ok true)
      (loop [directive :in schema :while ok]
        (set ok
             (match
               (protect
                 (cond
                   (fn? directive) (directive data)
                   (dictionary? directive)
                   (all truthy?
                        (seq [pred :pairs directive]
                          (match pred
                            [(fun (fn? fun)) (afun (fn? afun))]
                            (let [res (fun data)] (afun res))
                            [key (fun (fn? fun))]
                            (fun (get data key)))))))
               [true res] res
               [false _] false)))
      (if ok data false))))

(def ??? `Alias for validator` validator)

(defn analyst
  ```
  Creates function which can be used for analysing the data structure.
  It has one argument schema. See `(doc schema)`
  The function returns the empty tuple when it is valid
  or data structure mimicking the schema, with nonconforming members
  and predicate, that failed.
  ```
  [& schema]
  (fn analyst [data]
    (if ((validator ;schema) data)
      []
      (tuple
        ;(seq [directive :in schema]
           (match
             (protect
               (cond
                 (fn? directive) (if (directive data) () [data directive])
                 (dictionary? directive)
                 (let [res @{}]
                   (loop [pred :pairs directive]
                     (match pred
                       [(afun (fn? fun)) (fun (fn? afun))]
                       (if-not (fun (afun data)) (put res afun fun))
                       [key (fun (fn? fun))]
                       (if-not (fun (get data key)) (put res key fun))))
                   (freeze res))))
             [true r] r
             [false e] [directive [:error e]]))))))

(def !!! `Alias for analyst` analyst)

# Predicates
(defn present?
  ```
  Returns `true` if `value` is not falsey and is not empty.
  ```
  [value]
  (truthy? (and value (lengthable? value) (not (empty? value)))))

(def epoch? "Alias for number?" number?)

(defn present-string?
  ```
  Returns `true` if value is `present?` and is `string`
  ```
  [value]
  (and (string? value) (present? value)))

(defn string-number?
  ```
  Returns `true` if `value` is `present?` string and
  can be parsed to number
  ```
  [value]
  (and (present-string? value) (not (nil? (scan-number value)))))

# Functions
(defmacro ?one-of
  ```
  Returns function that returns `value` if its argument `value`
  is one of `values`.
  ```
  [& values]
  (def name (symbol 'one-of- (string/join (map symbol values) "-")))
  (with-syms [value]
    ~(fn ,name [,value] (find |(= ,value $) [,;values]))))

(defmacro ?gt
  ```
  Returns function that checks if the arument `i` is greater
  than `what`.
  ```
  [what]
  (def name (symbol 'gt- (describe what)))
  (with-syms [i] ~(fn ,name [,i] (,> ,i ,what))))

(defmacro ?gte
  ```
  Returns function that checks if the arument `i` is greater
  than or equal to `what`.
  ```
  [what]
  (def name (symbol 'gte- (describe what)))
  (with-syms [i] ~(fn ,name [,i] (,>= ,i ,what))))

(defmacro ?lt
  ```
  Returns function that checks if the arument `i` is less
  than `what`.
  ```
  [what]
  (def name (symbol 'lt- (describe what)))
  (with-syms [i] ~(fn ,name [,i] (,< ,i ,what))))

(defmacro ?lte
  ```
  Returns function that checks if the arument `i` is less
  than or equal to `what`.
  ```
  [what]
  (def name (symbol 'lte- (describe what)))
  (with-syms [i] ~(fn ,name [,i] (,<= ,i ,what))))

(defmacro ?eq
  ```
  Returns function that checks if the argument `i` is equal
  to `what`.
  ```
  [what]
  (def name (symbol 'eq- (describe what)))
  (with-syms [i] ~(fn ,name [,i] (,= ,what ,i))))

(defmacro ?deep-eq
  ```
  Returns function that checks if the argument `i` is deep equal
  to `what`.
  ```
  [what]
  (def name (symbol 'deep-eq- (describe what)))
  (with-syms [i]
    ~(fn ,name [,i] (,deep= ,what ,i))))

(defmacro ?matches
  ```
  Returns function that matches its arguments
  against the cases, same as if you used core match.
  ```
  [& cases]
  (with-syms [i] ~(fn matches? [,i] (match ,i ,;cases))))

(defn ?matches-peg
  ```
  Returns function that matches its arguments
  against the peg `pg`, and returns the matched.
  ```
  [pg]
  (fn matches-peg? [i] (peg/match pg i)))

(defmacro ?has-key
  ```
  Returns function, which when called with the dictionary
  returns `true`, if the dictionary has `key`
  ```
  [key]
  (def name (symbol 'has-key- key))
  (with-syms [i] ~(fn ,name [,i] (,not= nil (get ,i ,key)))))

(defmacro ?lacks-key
  ```
  Returns function, which when called with the dictionary
  returns `true`, if the dictionary lacks `key`
  ```
  [key]
  (def name (symbol 'lacks-key- key))
  (with-syms [i]
    ~(fn ,name [,i] (,= nil (get ,i ,key)))))

(defmacro ?has-keys
  ```
  Returns function, which when called with the dictionary
  returns `true`, if the dictionary argumen has all `keyz`.
  ```
  [& keyz]
  (def name (symbol 'has-keys- (string/join (map symbol keyz) "-")))
  (def kfns (map (fn [k] (fn [i] (not= nil (get i k)))) keyz))
  (with-syms [dictionary]
    ~(fn ,name [,dictionary] (all |($ ,dictionary) ,kfns))))

(defmacro ?lacks-keys
  ```
  Returns function, which returns `true`, if the dictionary argument
  lacks some `keyz`.
  ```
  [& keyz]
  (def name (symbol 'lacks-keys- (string/join keyz "-")))
  (def kfns (map (fn [k] (fn [i] (= nil (get i k)))) keyz))
  (with-syms [dictionary]
    ~(fn ,name [,dictionary] (some |($ ,dictionary) ,kfns))))

(defmacro ?num-in-range
  ```
  Returns function that checks if the argument is in
  range specified by `boundaries` not inclusive.
  One boundary is used as high and low is set to zero.
  ```
  [& boundaries]
  (def i (gensym))
  (def name (symbol 'num-in-range- (string/join (map string boundaries) "-")))
  (case (length boundaries)
    1 ~(fn ,name [,i] (< ,i ,(first boundaries)))
    2 ~(fn ,name [,i] (< ,(first boundaries) ,i ,(last boundaries)))))

(defmacro ?long
  "Returns function that checks if its argument has the length l"
  [l]
  (def name (symbol 'long- (describe l)))
  (with-syms [i]
    ~(fn ,name [,i] (= (length ,i) ,l))))

(defn ?prefix
  "Returns function that checks if `item` has prefix `pfx`."
  [pfx]
  (fn prefix? [item] (string/has-prefix? pfx item)))

(defn ?suffix
  "Returns function that checks if `item` has suffix `pfx`."
  [pfx]
  (fn suffix? [item] (string/has-suffix? pfx item)))

(defn ?find
  "Returns function that checks if `item` contains `part`."
  [& parts]
  (if (one? (length parts))
    (fn find? [item] (string/find (parts 0) item))
    (fn find? [item]
      (var start 0)
      (loop [part :in parts]
        (if (set start (string/find part item start))
          (+= start (length part))
          (break)))
      start)))

# Selectors
(defmacro from-to
  "Returns function that slice its argument `from` `to`"
  [from to &opt fn-name]
  (default fn-name (symbol 'from-to "-" from "-" to))
  (with-syms [xs xsl]
    ~(fn ,fn-name [,xs]
       (def ,xsl (length ,xs))
       (if (or (> ,from ,xsl)
               (> (math/abs ,to) ,xsl))
         []
         (slice ,xs ,from ,to)))))

(def rest
  "Selector that returns its argument without the first member"
  (from-to 1 -1))

(def butlast
  "Selector that returns its argument without the last member"
  (from-to 0 -2))

(defmacro def?!
  "Defines both validator and analyst for the `schema`, named `name?` and `name!`."
  [name & schema]
  ~(upscope
     (def ,(symbol name "?") ,(string name " validator") (,??? ,;schema))
     (def ,(symbol name "!") ,(string name " analyst") (,!!! ,;schema))))

(defmacro assert?!
  "Defines assert with message of analyst"
  [schema entity]
  ~(assert (,(symbol schema "?") ,entity) (string/format "%Q" (,(symbol schema "!") ,entity))))
