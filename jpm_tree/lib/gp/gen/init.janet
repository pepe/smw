(import spork/path)
(import spork/temple)
(import ../events :prefix "" :export true)
(temple/add-loader)

(def licenses
  "All licences"
  ["agpl3-header"
   "agpl3"
   "apache-header"
   "apache"
   "bsd2"
   "bsd3"
   "cc0-header"
   "cc0"
   "cc_by-header"
   "cc_by"
   "cc_by_nc-header"
   "cc_by_nc"
   "cc_by_nc_nd-header"
   "cc_by_nc_nd"
   "cc_by_nc_sa-header"
   "cc_by_nc_sa"
   "cc_by_nd-header"
   "cc_by_nd"
   "cc_by_sa-header"
   "cc_by_sa"
   "cddl"
   "epl"
   "gpl2"
   "gpl3-header"
   "gpl3"
   "isc"
   "lgpl"
   "mit"
   "mpl-header"
   "mpl"
   "unlicense"
   "wtfpl-header-warranty"
   "wtfpl-header"
   "wtfpl"
   "x11"
   "zlib"])

(defn set-config
  "Creates event that sets config in state"
  [config]
  (make-update (fn [_ state] (put state :config config)) "set-config"))

(defn log
  "Creates event that prints msgs to stderr"
  [& msgs]
  (make-effect
    (fn [&] (eprint ;msgs))))

(define-watch ListLicenses
  "Creates event that prints licences to stderr"
  [&]
  (seq [l :in licenses] (log l)))

(defn author
  "Gets author name and email from git"
  []
  (def res @"")
  (def np (os/spawn ["git" "config" "user.name"] :p {:in :pipe :out :pipe}))
  (if (not (zero? (:wait np))) (error "Error getting user from git"))
  (buffer/push res (string/trim (ev/read (np :out) :all)))
  (buffer/push res " <")
  (def ep (os/spawn ["git" "config" "user.email"] :p {:in :pipe :out :pipe}))
  (if (not (zero? (:wait ep))) (error "Error getting email from git"))
  (buffer/push res (string/trim (ev/read (ep :out) :all)))
  (buffer/push res ">")
  (freeze res))

(defn mkdir
  "Creates event that creates directory with name"
  [name]
  (make-effect (fn [&] (os/mkdir name)) (string "mkdir-" name)))

(defmacro- render-file [dir file template args]
  (def f (gensym))
  ~(with [,f (file/open (path/join ,dir ,file) :wn)]
     (with-dyns [:out ,f]
       ((get-in (require ,template) ['render-dict :value]) ,args))))

(defn render
  "Creates event that renders `template` to `dir`/`file`"
  [dir file template &opt args]
  (make-effect
    (fn [_ {:config c} _]
      (default args c)
      (render-file dir file (path/join "gp/gen/templates" template) args))
    (string "render-" dir "-" file "-" template)))

(defn prnmkdir
  "Creates event that creates directory `name`/`dir`"
  [name dir]
  (make-watch
    (fn [&]
      (def p (path/join name dir))
      [(log "  - " dir) (mkdir p)])
    (string "print-mkdir-" name "-" dir)))

(defn executable
  "Creates event that marks file on `path` as executable"
  [path]
  (make-effect (fn [&] (os/chmod path "rwx------")) (string "chmod-" path)))

(defn touch
  "Creates event taht touches file on `path`"
  [path]
  (make-effect
    (fn [&]
      (if (os/stat path)
        (os/touch path)
        (spit path "")))
    (string "touch-" path)))
