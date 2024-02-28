(use spork/htmlgen spork/misc)

(defmacro- def-res-push
  "Convenience macro for defining `res` array and function that pushes to it."
  []
  ~(upscope
     (def res @[])
     (defn push-res [& x] (array/push res ;x))))

(defn- _axis
  ```
  Renders optional axis.
  This function is called when you call `:axis` method on chart
  ```
  [chart & config]
  (:config chart config)
  (def {:height h :width w} chart)
  (def-res-push)
  (push-res :g {:class "axis"})
  (push-res
    [:line {:x1 0 :y1 0 :x2 0 :y2 h}]
    [:line {:x1 0 :y1 h :x2 w :y2 h}])
  (when-let [u (chart :unit)
             {:data-length dl :item-width iw :item-height ih
              :data-max dmx :data-min dmn :extreme ex :zero zo} chart
             lh (* h (get chart :unit-ratio 0.01))
             lw (* w (get chart :unit-ratio 0.01))]
    (push-res
      [:symbol {:id "hdot" :width lh :height 1}
       [:line {:x1 0 :y1 0 :x2 lh :y2 0}]]
      [:symbol {:id "vdot" :width 1 :height lh}
       [:line {:x1 0 :y1 0 :x2 0 :y2 lh}]])
    (loop [dy :range [(- h zo) zo (* u ih)]]
      (push-res [:use {:href "hdot" :x 0 :y dy}]))
    (loop [dx :down [w 0 (* u iw)]]
      (push-res [:use {:href "vdot" :x dx :y (- h lw)}])))
  (:push-content chart res)
  chart)

(defn- _process
  "Processes data and sets instance fields."
  [chart]
  (if-not (chart :processed)
    (let [{:d d :height height :width width} chart
          dl (length d) dmn (min 0 ;d) dmx (max 0 ;d)
          ex (- dmx dmn) ih (/ height ex)]
      (merge-into chart
                  {:processed true
                   :data-length dl :data-max dmx :data-min dmn
                   :extreme ex :item-height ih
                   :zero (- height (* -1 dmn ih))}))
    chart))

(defn- _bar
  "Renders bar chart"
  [chart & config]
  (:config chart config)
  (if (chart :content)
    (if-let [d ((:process chart) :d)]
      (:push-content
        chart (let [{:d d :height height :width width
                     :data-length dl :item-height ih
                     :data-max dmx :data-min dmn :extreme ex :zero zo} chart
                    iw (/ width dl)]
                (put chart :item-width iw)
                (def-res-push)
                (push-res :g {:class "chart bar"})
                (loop [[i p] :pairs d
                       :let [ph (math/abs (* p ih))
                             x (* i iw)
                             y (if (pos? p) (- zo ph) zo)]]
                  (push-res [:rect {:x x :y y :width iw :height ph}]))
                res))
      (error "No data to chart"))
    (error "No content to construct the chart in"))
  chart)

(defn- _spark
  "Renders spark chart"
  [chart & config]
  (:config chart config)
  (if (chart :content)
    (if-let [d ((:process chart) :d)]
      (:push-content
        chart (let [{:d d :height height :width width
                     :data-length dl :item-height ih
                     :data-max dmx :data-min dmn :extreme ex :zero zo} chart
                    iw (/ width (dec dl))]
                (put chart :item-width iw)
                (def-res-push)
                (push-res :g {:class "chart spark"})
                (def points
                  (seq [[i p] :pairs d
                        :let [ph (math/abs (* p ih))
                              x (* i iw)
                              y (if (pos? p) (- zo ph) zo)]]
                    (string x ", " y)))
                (push-res
                  [:polyline {:points (string/join points " ")}]
                  [:polygon
                   {:points
                    (string/join [;points
                                  (string width ", " height)
                                  (string "0, " height)] " ")}])
                res))
      (error "No data to chart"))
    (error "No content to construct the chart in"))
  chart)

(defn- _svg
  "Sets svg chart"
  [chart & config]
  (def tconf (table ;config))
  (merge-into chart tconf)
  (put chart :content
       @[:svg (merge {:version "1.1"
                      :xmlns "http://www.w3.org/2000/svg"}
                     tconf)]))

(def Chart
  "Prototype for a Chart."
  @{:svg _svg
    :bar _bar
    :spark _spark
    :axis _axis
    :config |(merge-into $0 (table ;$1))
    :process _process
    :push-content |(update $0 :content array/push $1)
    :style |(:push-content $0 [:style $1])
    :render |(freeze ($ :content))})
