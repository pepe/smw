(import spork/pgp)
(use spork/misc)

(defn words-id
  "Converts string to pgp word of its hash"
  [str]
  (-> str hash math/abs (int->string 16) pgp/hexs->words (string/join "-")))

(defn parse-presentation
  ```
  Parses file with the presentation slides and returns data structure with them as htmlgen
  ```
  [content]
  (defn <title
    [capture]
    {:title capture
     :uuid (words-id capture)
     :content content})
  (defn <ahref
    [l h]
    [:a {:href h} l])
  (defn <img
    [s]
    [:img {:src s}])
  (defn <chapter
    [frontmatter & slides]
    (put (table ;frontmatter)
         :slides slides))
  (defn <presentation
    [res & chapters]
    (merge res {:chapters (array ;chapters)}))
  (def grammar
    ~{:eol (+ "\n" "\r\n")
      :2eol (* :eol :eol)
      :h1 (* "#" :s* '(to :eol))
      :h2 (* "##" :s* (group (* (constant :h2) '(to :eol))))
      :div (+ "---" "===")
      :title (cmt :h1 ,<title)
      :to:ol (some (if-not (+ ":" :eol) 1))
      :kv (* (cmt ':to:ol ,keyword) ":" :s* '(to :eol) :eol)
      :frontmatter (group (* :s* :div :eol (some :kv) :div))
      :text (* :2eol (group (* (constant :p) '(to :2eol))) (thru :div))
      :empty (* :2eol :div)
      :bullet (group (* "*" :s* (constant :li) '(to :eol) :eol))
      :bullets (* :2eol (group (* (constant :ul) (some :bullet))) (thru :div))
      :link (* :2eol (cmt (* (* "[" '(to "]") "]" "(" '(to ")") ")")) ,<ahref)
               (thru :div))
      :img (* :2eol (cmt (* (* "!(" '(to ")") ")")) ,<img) (thru :div))
      :slide (group (* :s* (constant :section) :h2
                       (+ :empty :link :img :bullets :text)))
      :slides (some :slide)
      :chapter (cmt (* :frontmatter :slides) ,<chapter)
      :main (cmt (* :title (some :chapter)) ,<presentation)})
  (match (peg/match grammar content)
    [(res (table? res))] (freeze res)
    (error "content is not parsable presentation")))
