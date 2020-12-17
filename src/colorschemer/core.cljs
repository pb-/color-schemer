(ns ^:figwheel-hooks colorschemer.core
  (:require [dumdom.core :as dumdom :refer [defcomponent]]
            [goog.functions :refer [debounce]]))

(defn make-hue [hue]
  {:hue hue
   :shades [[0.80 0.35]
            [0.75 0.8]
            [0.08 0.97]]})

(defonce global-state
  (atom {:code? false
         :editing-name? false
         :selected 0
         :hues [(make-hue (rand-int 360))]
         :url (str js/window.location.origin js/window.location.pathname)}))

(defn format-fragment [data]
  (-> data
      clj->js
      js/JSON.stringify
      js/encodeURI))

(defn parse-fragment [data]
  (-> data
      js/decodeURI
      js/JSON.parse
      (js->clj :keywordize-keys true)))

(defn hsv->hsl [[hue saturation value]]
  (let [h hue
        l (* value (- 1 (/ saturation 2)))
        s (if (#{0 1} l) 0 (/ (- value l) (min l (- 1 l))))]
    [h s l]))

(defn hsl->css [[h s l]]
  (str "hsl(" h \, (js/Math.round (* 100 s)) "%," (js/Math.round (* 100 l)) "%)"))

(def hsv->css (comp hsl->css hsv->hsl))

(defn distant-hue [hues]
  (let [sorted (vec (sort hues))
        padded (conj sorted (+ 360 (first sorted)))
        pairs (map vector padded (next padded))
        sorted-pairs (sort-by #(- (second %) (first %)) pairs)
        [low high] (last sorted-pairs)]
    (mod (+ low (quot (- high low) 2)) 360)))

(defn add-hue [state]
  (-> state
      (update :hues conj (make-hue (distant-hue (map :hue (:hues state)))))
      (assoc :selected (count (:hues state)))))

(defn remove-hue [{:keys [selected hues] :as state}]
  (-> state
      (assoc :hues (vec (concat (subvec hues 0 selected) (subvec hues (inc selected) (count hues)))))
      (assoc :selected (max 0 (dec selected)))))

(defn select-hue [state selected]
  (assoc state :selected selected))

(defn average-shade [[s-1 v-1] [s-2 v-2]]
  (let [round (fn [x] (/ (js/Math.round (* 100 x)) 100))]
    [(round (/ (+ s-1 s-2) 2)) (round (/ (+ v-1 v-2) 2))]))

(defn interleave+
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (if (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave+ (rest s1) (rest s2))))
          (when s1
            s1))))))

(defn add-shades [shades]
  (let [new-shades (map average-shade shades (next shades))]
    (into [] (interleave+ shades new-shades))))

(defn remove-shades [shades]
  (into [] (take-nth 2 shades)))

(defn representative [hue]
  (let [shades (:shades hue)]
    (into [] (cons (:hue hue)
                   (nth shades (bit-shift-right (count shades) 1))))))

(defn shade-nums [shade-count]
  (case shade-count
    3 (range 1 10 4)
    5 (range 1 10 2)
    9 (range 1 10 1)))

(defn hue-info->css [{:keys [identifier hue shades]}]
  (apply str
         (for [[shade-num [s v]] (map vector (shade-nums (count shades)) shades)]
           (str "  --color-" identifier "-s" shade-num ": " (hsv->css [hue s v]) ";\n"))))

(defn with-identifier [index hue-info]
  (assoc
    hue-info :identifier
    (if-let [name- (:name hue-info)]
      (apply str (interpose \- (map #(.toLowerCase %) (re-seq #"\w+" name-))))
      (str \h (inc index)))))

(defn state->css [{:keys [hues url]}]
  (str ":root {\n"
       "  /* view/edit at " url "#" (format-fragment hues) " */\n"
       "  /* use these colors with var(--color-h1-s1) etc. */\n"
       (apply
         str
         (interpose
           \newline
           (map-indexed (comp hue-info->css with-identifier) hues)))
       "}\n"))

(defn hue-name [hue-info]
  (or (:name hue-info) (str "Hue " (:hue hue-info))))

(defn name-hue [new-name hue-info]
  (let [n (.trim new-name)]
    (cond
    (empty? n) (dissoc hue-info :name)
    (= n (hue-name hue-info)) hue-info
    :else (assoc hue-info :name n))))

(defn finish-editing [state new-name]
  (-> state
      (update-in [:hues (:selected state)] (partial name-hue new-name))
      (assoc :editing-name? false)))

(defn focus-element! [e]
  (when e (.focus e)))

(defcomponent slider
  [value low high step class- on-change]
  [:input {:type :range
           :value value
           :min low
           :max high
           :step step
           :class class-
           :onInput (fn [e] (on-change (js/Number (.-target.value e))))}])

(defcomponent percentage [p]
  [:span (str (js/Math.round (* 100 p)) \%)])

(defcomponent hue-detail-controls [{:keys [hue shades] :as hue-info} on-change]
  [:div
   [slider hue 0 359 1 "hue-slider" (fn [h] (on-change (fn [hue-info] (assoc hue-info :hue h))))]
   [:div {:class "shade-grid"
          :style {:grid-template-rows (str "repeat(" (count shades) ", 2rem)")}}
    (apply
      concat
      (for [[i [[saturation value] shade-num]]
            (map-indexed vector (map vector shades (shade-nums (count shades))))]
        [[:span (str "Shade #" shade-num)]
         [slider saturation 0 1 0.01 nil
          (fn [s] (on-change (fn [hue-info] (assoc-in hue-info [:shades i 0] s))))]
         [percentage saturation]
         [slider value 0 1 0.01 nil
          (fn [v] (on-change (fn [hue-info] (assoc-in hue-info [:shades i 1] v))))]
         [percentage value]
         [:div {:class "shade-swatch"
                :style {:background-color (hsv->css [hue saturation value])}}]]))]])

(defcomponent hue-details [{:keys [selected hues editing-name?] :as state} on-change]
  (let [hue-info (hues selected)]
    [:div
     (if editing-name?
       [:input {:value (:name hue-info)
                :class "name-input"
                :ref focus-element!
                :placeholder "Hue name"
                :onKeyUp #(when (= (.-key %) "Enter")
                            (on-change (fn [s] (finish-editing s (.-target.value %)))))
                :onBlur #(on-change (fn [s] (assoc s :editing-name? false)))}]
       [:h1 {:class "hue-headline"
             :style {:color (hsv->css (representative hue-info))}}
        (hue-name hue-info)
        (when (> (count hues) 1)
          [:span {:class "name-control"
                  :title "Remove hue"
                  :onClick #(on-change remove-hue)} "✕"])
        [:span {:class "name-control"
                :title "Change name"
                :onClick #(on-change (fn [s] (assoc s :editing-name? true)))} "✎"]])
     [hue-detail-controls
      hue-info
      (fn [hue-info] (on-change (fn [s] (update-in s [:hues (:selected s)] hue-info))))]]))

(defcomponent hue-swatch [color on-click]
  [:div {:class "hue-swatch"
         :style {:background-color (hsv->css color)}
         :onClick on-click}])

(defcomponent hue-swatch-glue [{:keys [color position]}]
  [:div {:class "hue-swatch-glue"
         :style {:grid-column position
                 :background-color (hsv->css color)}}])

(defcomponent css-code [state]
  [:pre {:class "code"
         :mounted-style {:opacity "1"}}
   (state->css state)])

(defcomponent editor [{:keys [selected hues] :as state} on-change]
  (let [hue (hues selected)]
    [:div {:class "editor"
           :mounted-style {:opacity "1"}}
     [:div {:class "swatch-navigation"
            :style {:grid-template-columns (str "repeat(" (inc (count hues)) ", 2rem)")
                    :border-bottom-color (hsv->css (representative hue))}}
      (for [[i hue] (map-indexed vector hues)]
        [hue-swatch (representative hue) (fn [e] (on-change (fn [s] (select-hue s i))))])
      [:div {:class "add-hue"
             :onClick (fn [e] (on-change (fn [s] (add-hue s))))} [:span \+]]
      [hue-swatch-glue {:color (representative hue) :position (inc selected)}]]
     [hue-details state on-change]
     [:div {:class "shade-controls"}
      (when (> (count (:shades hue)) 3)
        [:button
         {:onClick (fn [e] (on-change (fn [s] (update-in s [:hues (:selected s) :shades] remove-shades))))
          :class "fewer-shades"}
         "Fewer shades"])
      (when (< (count (:shades hue)) 9)
        [:button
         {:onClick (fn [e] (on-change (fn [s] (update-in s [:hues (:selected s) :shades] add-shades))))
          :class "more-shades"}
         "More shades"])]]))

(defcomponent main [{:keys [code?] :as state} on-change]
  [:div {:class "main"}
   [:div {:class "header"}
    [:span "An editor for "
     [:a {:href "https://refactoringui.com/previews/building-your-color-palette/"} "this approach"] \.]
    [:button {:onClick (fn [e] (on-change (fn [s] (update s :code? not))))}
     (if code? "Back to editor" "Link / Export CSS")]]
   (if code?
     [css-code state]
     [editor state on-change])
   [:p {:class "source"} "Version 5. Show me "
    [:a {:href "https://github.com/pb-/color-schemer/blob/main/src/colorschemer/core.cljs"} "the source"] \.]])

(defn update-favicon! []
  (let [canvas (js/document.getElementById "favicon-canvas")
        link (js/document.getElementById "favicon-link")
        ctx (.getContext canvas "2d")
        state @global-state
        hues (:hues state)
        hue-count (count hues)
        palette-size (min 3 (js/Math.ceil (js/Math.sqrt hue-count)))
        swatch-size (quot 16 palette-size)
        hue ((:hues state) (:selected state))]
    (.clearRect ctx 0 0 16 16)
    (doseq [i (range palette-size)
            j (range palette-size)
            :let [index (+ i (* palette-size j))]
            :when (< index hue-count)]
      (set! (. ctx -fillStyle) (hsv->css (representative (hues index))))
      (.fillRect ctx (* i swatch-size) (* j swatch-size) swatch-size swatch-size))
    (set! (. link -href) (.toDataURL canvas "image/png"))))

(def update-favicon-lazy! (debounce update-favicon! 250))

(defn render! []
  (let [state @global-state]
    (dumdom/render
      [main state (fn [f] (swap! global-state f) (render!))]
      (js/document.getElementById "app"))
    (update-favicon-lazy!)))

(defn init! []
  (when (not-empty js/window.location.hash)
    (try
      (swap! global-state assoc :hues (parse-fragment (subs js/window.location.hash 1)))
      (catch js/Error _))
    (js/history.replaceState nil nil " ")) ; clear URL fragment
  (render!)
  (update-favicon!))

(init!)
