(use spork/misc)
(import gp/data/fuzzy :prefix "" :export true)

(defn score-n-order
  ```
  Takes `needle` with the fuzzy search and `hays` with collection of the tuples
  of the form `[string score]` it then fuzzy scores string and
  returns the collection of the same shape, but only with the results 
  that matched and with updated `score`.
  ```
  [needle hays]
  (do-def res @[]
          (loop [[hay _ _] :in hays
                 :let [sc (score needle hay)]
                 :when (and sc (> sc score-min))]
            (array/insert res (find-index |(> sc ($ 1)) res (length res))
                          [hay sc]))))
