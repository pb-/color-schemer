(ns ^:figwheel-hooks colorschemer.core
  (:require [dumdom.core :as dumdom :refer [defcomponent]]))

(defn make-hue []
  {:hue (rand-int 361)
   :shades [[0.80 0.35]
            [0.75 0.8]
            [0.08 0.97]]})

(defonce global-state
  (atom {:code? false
         :selected 0
         :hues [(make-hue)]}))

(defn hsv->hsl [[hue saturation value]]
  (let [h hue
        l (* value (- 1 (/ saturation 2)))
        s (if (#{0 1} l) 0 (/ (- value l) (min l (- 1 l))))]
    [h s l]))

(defn hsl->css [[h s l]]
  (str "hsl(" h \, (js/Math.round (* 100 s)) "%," (js/Math.round (* 100 l)) "%)"))

(def hsv->css (comp hsl->css hsv->hsl))

(defn add-hue [state]
  (-> state
      (update :hues conj (make-hue))
      (assoc :selected (count (:hues state)))))

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

(defcomponent hue-details [{:keys [hue shades] :as hue-info} on-change]
  [:div
   [:h1 {:class "hue-headline"
         :style {:color (hsv->css (representative hue-info))}} (str "Hue " hue)]
   [slider hue 0 360 1 "hue-slider" (fn [h] (on-change (fn [hue-info] (assoc hue-info :hue h))))]
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

(defcomponent hue-swatch [color on-click extra-style]
  [:div {:class "hue-swatch"
         :style (merge {:background-color (hsv->css color)} extra-style)
         :onClick on-click}])

(defcomponent hue-swatch-glue [color selected]
  [:div {:class "hue-swatch-glue"
         :style {:grid-column (inc selected)
                 :background-color (hsv->css color)}}])

(defn hue-info->css [{:keys [identifier hue shades]}]
  (apply str
         (for [[shade-num [s v]] (map vector (shade-nums (count shades)) shades)]
           (str "  --color-" identifier "-s" shade-num ": " (hsv->css [hue s v]) ";\n"))))

(defn state->css [{:keys [hues]}]
  (str ":root {\n"
       "  /* use these colors with var(--color-h1-s1) etc. */\n"
       (apply
         str
         (interpose
           \newline
           (map-indexed #(hue-info->css (assoc %2 :identifier (str \h (inc %1)))) hues)))
       "}\n"))

(defcomponent css-code [state]
  [:pre {:class "code"
         :mounted-style {:opacity "1"}}
   (state->css state)])

(defcomponent editor [{:keys [selected hues]} on-change]
  (let [hue (hues selected)]
    [:div {:class "editor"
           :mounted-style {:opacity "1"}}
     [:div {:class "swatch-navigation"
            :style {:grid-template-columns (str "repeat(" (inc (count hues)) ", 2rem)")
                    :border-bottom-color (hsv->css (representative hue))}}
      (for [[i hue] (map-indexed vector hues)]
        [hue-swatch (representative hue) (fn [e] (on-change (fn [s] (select-hue s i)))) {}])
      [:div {:class "add-hue"
             :onClick (fn [e] (on-change (fn [s] (add-hue s))))} [:span \+]]
      [hue-swatch-glue (representative hue) selected]]
     [hue-details
      hue
      (fn [hue-info] (on-change (fn [s] (update-in s [:hues (:selected s)] hue-info))))]
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
    [:span "A color-scheme editor for "
     [:a {:href "https://refactoringui.com/previews/building-your-color-palette/"} "this approach"] \.]
    [:button {:onClick (fn [e] (on-change (fn [s] (update s :code? not))))}
     (if code? "Back to editor" "Export CSS")]]
   (if code?
     [css-code state]
     [editor state on-change])
   [:p {:class "source"} "Version 2. Show me "
    [:a {:href "https://github.com/pb-/color-schemer/blob/master/src/colorschemer/core.cljs"} "the source"] \.]])

(defn render! []
  (let [state @global-state]
    (dumdom/render
      [main state (fn [f] (swap! global-state f) (render!))]
      (js/document.getElementById "app"))))

(render!)
