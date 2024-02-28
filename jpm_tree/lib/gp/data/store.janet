(use spork/misc ./navigation)

(defn- image-file [self]
  (string (self :image) ".jimage"))

(defn save-image
  "Saves the `store` to the image file."
  [store]
  (spit (:image-file store)
        ((store :make-image) store)))

(defn load
  "Load an item on `path` from `store`."
  [{:root root} & path]
  (case (length path)
    0 root
    1 (in root (first path))
    (get-in root path)))

(defn transact
  "Transact traverse navigation `nav` on the `store`"
  [store & nav]
  (if (empty? nav)
    store
    ((traverse ;nav) (store :root))))

(defn save
  "Saves `what` on optional `path` to `store`, and index it."
  [store what & path]
  (if (empty? path)
    (put store :root what)
    (let [container
          (if (one? (length path))
            (store :root)
            (get-in store [:root ;(slice path 0 -2)]))]
      (put container (last path) what))))

(defn flush
  "Flushes store to the image file"
  [store]
  (save-image store)
  store)

(defn init
  "Initializes store"
  [self]
  (def imf (:image-file self))
  (merge-into
    self
    (if (os/stat imf)
      ((self :load-image) (slurp imf))
      @{:root @{}})
    {:image (self :image)})
  (flush self))

(def Store
  "Basic data store"
  @{:init init
    :flush flush
    :image "store"
    :load load
    :transact transact
    :save save
    :make-image make-image
    :load-image load-image
    :image-file image-file})
