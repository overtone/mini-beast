(ns minibeast.core
  (:import (javax.swing JFileChooser JMenuBar JMenu JMenuItem)
           (javax.swing.filechooser FileNameExtensionFilter)
           (java.awt.event ActionListener)
           (java.io File))
  (:use [overtone.live :exclude (mouse-button mouse-x mouse-y)]
        [quil.core :exclude (abs acos asin atan atan2 ceil cos
                                 exp line log
                                 ;;  mouse-button mouse-x mouse-y
                                 pow round scale sin sqrt tan triangle
                                 TWO-PI)]
        [minibeast.version :only [BEAST-VERSION-STR]]
        [clojure.set :only [difference]]
        [quil.applet]
        [minibeast.mbsynth]))

;; Create some synth instruments to be used by voices.
(def synths (doall (map (fn [x] (mbsynth)) [1 2 3 4 5 6 7 8])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal synth state
;;
;; These definitions store and maniupulate state internal
;; to the operation of the state of the synth.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord SynthState [sub-osc-waveform
                  sub-osc-amp
                  sub-osc-oct
                  lfo-waveform
                  lfo-amp
                  mod-wheel-fn
                  mod-wheel-pos
                  vibrato-fn
                  arp-mode
                  arp-range])

(def synth-state (ref (SynthState. :sub-osc-square 0.0 1 :lfo-sin 1.0 :cutoff 0.0 :vibrato 0 2)))

(defn alter-state [f & more]
  "Alters the state of the synth."
  (dosync
   (apply alter synth-state f more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal ui state
;;
;; These definitions store and maniupulate state internal
;; to the operation of the ui of the synth.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ui-state                   (atom {}))
(defonce selected-control           (atom nil))
(defonce dragged-control            (atom nil))
(defonce dev-chan-note-cmd->control (atom {}))
(defonce mouse-pressed-note         (atom nil))

;; Keep a record of the last eight key presses
;; in a map of {:synth s, :note n}
(def voices-max 8)
(defonce voices (ref ()))

(defn update-ui-state [m]
  (swap! ui-state merge m))

(defn ctl-ui-and-synth
  "Control ui and synth parameters

    [synth-ctls control-name ui-val]
       synth-ctls   - list of lists [[ctl val] [ctl val] ... ]
       control-name - the name of the control being modified
       ui-val       - 0-127 midi velocity

    [synth-ctl synth-val ui-val]
       synth-ctl    - synth parameter to modify
       synth-val    - value to use when modifying synth parameter
       control-name - the name of the control being modified
       ui-val       - 0-127 midi velocity"
  ([synth-ctls control-name ui-val]
     (doall (map (fn [[synth-ctl synth-val]] (ctl-ui-and-synth synth-ctl synth-val control-name ui-val)) synth-ctls)))

  ([synth-ctl synth-val control-name ui-val]
     (update-ui-state {control-name  ui-val})
     (ctl (map :id synths) synth-ctl synth-val)))

(def sub-osc-waveforms [:sub-osc-square :sub-osc-sin])
(defn next-sub-osc-waveform [w]
  "Given a sub-octave waveform, return the next waveform that can be selected."
  (let [waveforms (cycle sub-osc-waveforms )]
    (nth waveforms (inc (.indexOf waveforms w)))))

(def lfo-waveforms [:lfo-sin :lfo-tri :lfo-saw :lfo-square :lfo-rand :lfo-slew-rand])
(defn next-lfo-waveform [w]
  "Given an lfo waveform, return the next waveform that can be selected."
  (let [waveforms (cycle lfo-waveforms )]
    (nth waveforms (inc (.indexOf waveforms w)))))

(def mod-wheel-fns [:cutoff :vibrato :lfo-amount])
(defn next-mod-wheel-fn [f]
  "Given mod wheel fn, return the next fn that can be selected."
  (let [fns (cycle mod-wheel-fns)]
    (nth fns (inc (.indexOf fns f)))))

(def vibrato-fns [:vibrato :trill-up :trill-down])
(defn next-vibrato-fn [f]
  "Given vibrato fn, return the next fn that can be selected."
  (let [fns (cycle vibrato-fns)]
    (nth fns (inc (.indexOf fns f)))))

(defn queue [q e]
  "Add the element e at the end of the queue q."
  (dosync
   (alter q conj e)))

(defn dequeue [q]
  "Remove the least recently added element from the queue q."
  (dosync
   (let [e (first @q)]
     (alter q rest)
     e)))

(defn remove-first-match [pred q]
  "Remove the element satisfying (pred e) from the queue q."
  (dosync
   (let [s (group-by pred (ensure q))
         t (rest (s true)) ;; take off the head of the list of matched elements
         f (s false)]      ;; keep the unmatched elements
     (ref-set q (concat t f))
     (first (s true)))))

(defn getsynth [note]
  "Find a free synth or reclaim an old one, add the note and synth to
  the voices list and return the synth that was free or reclaimed."
  (dosync
   (if (>= (count @voices) voices-max)
     (let [voice     (dequeue voices)
           new-voice (assoc voice :note note)]
       (queue voices new-voice)
       (:synth new-voice))
     (let [first-unused-synth (first (difference (set synths) (map (fn [e] (:synth e)) @voices)))
           new-voice          {:synth first-unused-synth :note note}]
       (queue voices new-voice)
       (:synth new-voice)))))


;; Find a synth and turn it on. Turn off an old synth if we need to free one up.
(defn keydown [note velocity]
  (dosync
    (if-not (some #(= (:note %) note) @voices)
      (let [synthid (getsynth note)]
        ;; turn off the old note. Maybe this is a reused synth
        (ctl synthid :gate 0.0)
        ;; turn on the new note
        (ctl synthid :note note)
        (ctl synthid :velocity velocity)
        (ctl synthid :gate 1.0)))))

;; Find a synth to turn off
(defn keyup [note]
  (if-let [voice (remove-first-match #(= (:note %) note) voices)]
    (ctl (:synth voice) :gate 0.0)))

(defn handle-control
  "Typically called with an incoming MIDI control message."
  [{{device-name :name} :device
    note                :note
    chan                :channel
    vel                 :velocity
    ts                  :timestamp
    msg                 :msg
    cmd                 :command
    :as control-msg}]
  ;; (println control-msg)
  (if (nil? @selected-control)
    ;; lookup a control matching the device, channel, note, and cmd
    (if-let [matched-control (@dev-chan-note-cmd->control
                              {:device-name device-name
                               :chan        chan
                               :note        note
                               :cmd         cmd})]
      ;; find the synth parameter this control controls
      (let [synth-ctls   ((:synth-fn matched-control) vel)
            control-name (:name matched-control)
            ui-val       ((:ui-fn matched-control) vel)]
        (ctl-ui-and-synth synth-ctls control-name ui-val)))
    (do
      (println "assigning assoc "
               {:device-name device-name :chan chan :note note :cmd cmd}
               @selected-control)
      ;; make the association between the midi event and the synth control
      (swap! dev-chan-note-cmd->control
             assoc
             {:device-name device-name :chan chan :note note :cmd cmd}
             @selected-control)
      ;; clear the selected control value
      (reset! selected-control nil))))

;; x,y screen postion of the control
;; type :knob, :slider, :selector
;; ui-hints map of hints to draw the control {pos-indicator? start-sym end-sym zero? caption}
;; synth-param specifier of which synth parameter to control
;; f function to transform midi velocity value to synth param value
(defrecord Control [x y type ui-hints ctl f])

(defrecord AdvancedControl [x y type ui-hints name synth-fn ui-fn])

(defmacro
  do-transformation
  "Saves current transformation, performs body, restores current
  transformation on exit."
  [& body]
  `(do
     (push-matrix)
     ~@body
     (pop-matrix)))

(defmacro
  let-transformation
  "Performs the bindings then performs do-transformation on the body"
  [bindings & body]
  `(let* ~(destructure bindings)
         (do-transformation ~@body)))

(def selected-tint [255 100 100])

(defn draw-key [x y key-color selected?]
  (if selected?
    (apply tint selected-tint)
    (tint (color 255 255 255 255)))
  (image (case key-color
           :white (state :white-key-img)
           :black (state :black-key-img))
        x y))

(defn draw-knob [x y amount selected? pos-indicator? start-sym end-sym
                 sym-dx sym-dy zero? caption caption-dx caption-dy]
  (let-transformation
      [w2                  (/ 50 2.0)
       tl                  [(+ x w2) (+ y w2)]
       knob-background-img (state :knob-background-img)
       knob-img            (state :knob-img)]
    (apply translate tl)
    (text-size 8)
    (text-align :center)
    (if pos-indicator?
      (image knob-background-img (- 0 w2) (- 1 w2)))
    (if-not (nil? start-sym)
      ;; draw start sym
      (text start-sym (- -23 (or sym-dx 0)) (+ 27 (or sym-dy 0))))
    (if-not (nil? end-sym)
      ;; draw end sym
      (text end-sym (+ 22 (or sym-dx 0)) (+ 27 (or sym-dy 0))))
    (if zero?
      ;; draw zero
      (text "0" 0 -27))
    (if-not (nil? caption)
      (let [cdx (or caption-dx 0)
            cdy (or caption-dy 0)]
        ;; draw caption
        (text caption (+ cdx 0) (+ cdy 30))))
    (rotate (- (* amount 0.038) 2.4))
    (translate (- w2) (- w2))
    (when selected?
      (apply tint selected-tint))
    (image knob-img 0 0)
    (tint 255 255 255)))

(defn draw-slider [x y amount selected? caption caption-dx caption-dy]
  "x: x position
     y: y position
     amount: 0.0-127.0"
  (let [slider-background-img (state :slider-background-img)
        slider-img            (state :slider-img)]
    (image slider-background-img (- x 5) (- y 77))
    (if-not (nil? caption)
      (let [cdx (or caption-dx 0)
            cdy (or caption-dy 0)]
        (text-size 8)
        (text-align :center)
        (text caption (+ 16 cdx x) (+ cdy y 40))))
    (if selected?
      (apply tint selected-tint))
    (image slider-img x (+ y (* -0.6  amount)))
    (tint 255 255 255)))

(defn draw-selector [x y pos selected? caption caption-dx caption-dy]
  "x: x position
     y: y position
     pos: y position offet"
  (let [selector-background-img (state :selector-background-img)
        selector-img            (state :selector-img) ]
    (image selector-background-img (+ x 1) (+ y 3))
    (if-not (nil? caption)
      (let [cdx (or caption-dx 0)
            cdy (or caption-dy 0)]
        (text-size 8)
        (text-align :center)
        (text caption (+ 10 cdx x) (+ y cdy 44))))
    (if selected?
      (apply tint selected-tint))
    (image selector-img x (+ y pos))
    (tint 255 255 255)))

(defn draw-wheel [x y amount selected? caption caption-dx caption-dy]
  "x: x position on screen
   y: y position on screen
   amount: 0.0-127.0
   selected?: draw control in selected mode"
  (let [wheel-dimple-background-img (state :wheel-dimple-background-img)
        wheel-img                   (state :wheel-img)
        wheel-dimple-img            (state :wheel-dimple-img)
        wheel-dimple-inv-img        (state :wheel-dimple-inv-img)]
    (if-not (nil? caption)
      (let [cdx (or caption-dx 0)
            cdy (or caption-dy 0)]
        (text-size 8)
        (text-align :center)
        (text caption (+ 23 cdx x) (+ y cdy 144))))
    (let [tcs (if selected?
                selected-tint
                [255 255 255])]
      (apply tint tcs)
      (image wheel-img x y)
      (let [dx (+ x 10)
            dy (+ y 103 (* -0.71 amount))
            t  (* 2 amount)]
        (image wheel-dimple-background-img dx dy)
        (apply tint (conj tcs t))
        (image wheel-dimple-img dx dy)
        (apply tint (conj tcs (- 255 t)))
        (image wheel-dimple-inv-img dx dy)
        (tint 255 255 255 255)))))

(defn draw-control [control]
  (let [selected? (= @selected-control control)
        ui-val    (or ((:name control) @ui-state) 0)
        args      (conj ((juxt :x :y #((:ui-fn %) ui-val)) control) selected?)
        ui-hints  (:ui-hints control)]
    (case (:type control)
      :knob (apply draw-knob (apply conj args
                                    ((juxt :pos-indicator? :start-sym :end-sym :sym-dx :sym-dy
                                           :zero? :caption :caption-dx :caption-dy) ui-hints)))
      :slider (apply draw-slider (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :selector (apply draw-selector (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :wheel (apply draw-wheel (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints))))
    (when-let [ui-aux-fn (-> control :ui-hints :ui-aux-fn )]
      (ui-aux-fn))))

(defn control->advanced-control [control]
  ;; [x y type ui-hints name synth-fn ui-fn]
  (AdvancedControl.
   (:x control)
   (:y control)
   (:type control)
   (:ui-hints control)
   (:ctl control)
   (fn [val] [[(:ctl control) ((:f control) val)]])
   identity))

(defn mk-ui-hint-builder
  [default]
  (fn builder
    ([caption] (builder caption {}))
    ([caption m]
       (merge default {:caption caption} m))))

(def mk-pos-only-knob
  "Returns a ui-hints map which is a merger between m and the default
  map"
  (mk-ui-hint-builder
   {:pos-indicator? true}))

(def mk-plus-minus-knob
  (mk-ui-hint-builder
   {:pos-indicator? true :start-sym "-" :end-sym "+" :zero? true}))


(defn selector-knob-label-pos
  "Return [x y] position of label for selector knob.
  x,y - position of knob.
  i - index of label. 0...n-1"
  [x y i]
  [(+ x 20 (* -29 (Math/cos (+ (/ i 2.0) 1.5))))
   (+ y 22 (* -27 (Math/sin (+ (/ i 2.0) 1.5))))])

(def controls
  (concat
   (map control->advanced-control
        [
         (Control. 885 38 :knob (mk-pos-only-knob "Master Volume")      :volume            (fn [val] (/ val 12.0)))
         (Control. 750 38 :knob (mk-pos-only-knob "Feedback Amt")       :feedback-amp      (fn [val] (/ val 127.0)))
         (Control. 545 35 :knob (mk-pos-only-knob "Cutoff")             :cutoff            (fn [val] (* (- val 10) 100.0)))
         (Control. 613 35 :knob (mk-pos-only-knob "Resonance")          :resonance         (fn [val] (/ val 32.0)))
         (Control. 479 35 :knob (mk-pos-only-knob "Tri fold")           :tri-fold-thresh   (fn [val] (+ (/ val -127.0) 1)))
         (Control. 341 35 :knob (mk-pos-only-knob "Detune Amt")         :saw-detune-amp    (fn [val] (/ val 127.0)))
         (Control. 409 38 :knob (mk-pos-only-knob "Pulse Width"
                                                  {:start-sym "50%"
                                                   :end-sym "90%"
                                                   :sym-dy -5})         :osc-square-pw     (fn [val] (+ (/ val 255.0) 0.5)))

         (Control. 320 265 :slider {}                                   :osc-saw           (fn [val] (/ val 127.0)))
         (Control. 360 265 :slider {}                                   :osc-square        (fn [val] (/ val 127.0)))
         (Control. 400 265 :slider {}                                   :osc-tri           (fn [val] (/ val 127.0)))
         (Control. 440 265 :slider {}                                   :osc-noise         (fn [val] (/ val 127.0)))

         (Control. 475 332 :knob (mk-plus-minus-knob "PWM")             :lfo2pwm           (fn [val] (- (/ val 32.0) 1.98)))
         (Control. 544 332 :knob (mk-plus-minus-knob "Pitch")           :lfo2pitch         (fn [val] (- (/ val 2.0) 32.0)))
         (Control. 612 332 :knob (mk-plus-minus-knob "Filter")          :lfo2filter        (fn [val] (* (- val 64.0) 400.0)))
         (Control. 681 332 :knob (mk-plus-minus-knob "Amp"
                                                     {:caption-dx -25
                                                      :caption-dy -55}) :lfo2amp           (fn [val] (- (/ val 32) 1.98)))

         (Control. 338 398 :knob (mk-pos-only-knob "Glide")             :portamento        (fn [val] (/ val 1270.0)))
         (Control. 408 398 :knob (mk-pos-only-knob "Rate")              :vibrato-rate      (fn [val] (/ val 8.0)))
         (Control. 546 398 :knob (mk-pos-only-knob "Rate")              :lfo-rate
                   (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 3.0)))

         (Control. 341 102 :knob (mk-pos-only-knob   "Detune Rate")     :saw-detune        (fn [val] (/ val 8.0)))
         (Control. 409 102 :knob (mk-plus-minus-knob "ENV Amt")         :osc-square-pw-env (fn [val] (- (/ val 64.0) 1.0)))
         (Control. 479 102 :knob (mk-plus-minus-knob "ENV Amt")         :tri-fold-env      (fn [val] (- (/ val 64.0) 1.0)))
         (Control. 545 102 :knob (mk-plus-minus-knob "ENV Amt")         :cutoff-env        (fn [val] (* (- val 64.0) 200.0)))
         (Control. 613 102 :knob (mk-plus-minus-knob "KBD Tracking"
                                                     {:start-sym "0%"
                                                      :end-sym "200%"
                                                      :sym-dy -5})      :cutoff-tracking   (fn [val] (/ val 64.0)))

         (Control. 60  332 :knob (mk-pos-only-knob "Level")             :delay-mix         (fn [val] (/ val 127.0)))
         (Control. 130 332 :knob (mk-pos-only-knob "F.Back")            :delay-feedback    (fn [val] (/ val 127.0)))
         (Control. 200 332 :knob (mk-pos-only-knob "Time")              :delay-time        (fn [val] (/ val 127.0)))

         (Control. 60  398 :knob (mk-pos-only-knob "Mix")               :reverb-mix        (fn [val] (/ val 127.0)))
         (Control. 130 398 :knob (mk-pos-only-knob "Size")              :reverb-size       (fn [val] (/ val 127.0)))
         (Control. 200 398 :knob (mk-pos-only-knob "Damp")              :reverb-damp       (fn [val] (/ val 127.0)))

         (Control. 565 265 :slider {:caption "Attack"}                  :filter-attack     (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
         (Control. 605 265 :slider {:caption "Decay"}                   :filter-decay      (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
         (Control. 645 265 :slider {:caption "Sustain"}                 :filter-sustain    (fn [val] (/ val 127.0)))
         (Control. 685 265 :slider {:caption "Release"}                 :filter-release    (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
         (Control. 770 265 :slider {:caption "Attack"}                  :amp-attack        (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
         (Control. 810 265 :slider {:caption "Decay"}                   :amp-decay         (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
         (Control. 850 265 :slider {:caption "Sustain"}                 :amp-sustain       (fn [val] (/ val 127.0)))
         (Control. 890 265 :slider {:caption "Release"}                 :amp-release       (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))

         (Control. 885 332 :knob (mk-pos-only-knob "Tempo")             :arp-rate          (fn [val] (let [rate (/ val 5.0)]
                                                                                                       (println "arp-rate " (* (/ 60 8) rate) " bmp")
                                                                                                       rate)))
         (Control. 829 398 :knob (mk-pos-only-knob "Swing")             :arp-swing-phase   (fn [val] (* 360 (/ val 127.0))))
         ]) [

        ;; Put advanced controls here [x y synth-fn ui-fn]
        ;; LFO waveform selector
        (AdvancedControl. 478 398 :knob {:caption   "Wave"
                                         :ui-aux-fn #(doall
                                                       (map-indexed
                                                         (fn [i e] (apply shape 
                                                                          (state e)
                                                                          (selector-knob-label-pos 478 398 i)))
                                                         [:sin-shape :tri-shape :saw-shape :square-shape
                                                          :random-shape :random-slew-shape]))}
                          :lfo-waveform
                          (fn [val] (let [old-waveform (:lfo-waveform @synth-state)
                                         new-state    (alter-state
                                                       #(assoc % :lfo-waveform
                                                               (if (= val 0)
                                                                 ;; button press; switch to next waveform
                                                                 (next-lfo-waveform (:lfo-waveform %))
                                                                 ;; knob or slider; calculate waveform
                                                                 (lfo-waveforms (int (* (/ val 128.0) (count lfo-waveforms)))))))
                                         new-waveform (:lfo-waveform new-state)]
                                     ;; Toggle lfo waveform
                                     [[old-waveform 0] [new-waveform (:lfo-amp @synth-state)]]))
                          (fn [val] (case (:lfo-waveform @synth-state)
                                     :lfo-sin 60 :lfo-tri 75 :lfo-saw 89 :lfo-square 100 :lfo-rand 115 :lfo-slew-rand 130)))

        ;; sub-octave osc waveform selector
        (AdvancedControl. 290 46 :selector {:caption   "WAVE"
                                            :ui-aux-fn (fn [] (shape (state :square-shape) 310 51)
                                                              (shape (state :sin-shape)    310 65))}
                          :sub-osc-waveform
                          (fn [val] (let [old-waveform (:sub-osc-waveform @synth-state)
                                         new-state    (alter-state
                                                       #(assoc % :sub-osc-waveform
                                                               (if (zero? val)
                                                                 ;; button press; switch to next waveform
                                                                 (next-sub-osc-waveform (:sub-osc-waveform %))
                                                                 ;; knob or slider; calculate waveform
                                                                 (sub-osc-waveforms (int (* (/ val 128.0) (count sub-osc-waveforms)))))))
                                         new-waveform (:sub-osc-waveform new-state)]
                                     ;; Toggle sub-osc waveform
                                     [[old-waveform 0] [new-waveform (:sub-osc-amp @synth-state)]]))
                          (fn [val] (case (:sub-osc-waveform @synth-state)
                                     :sub-osc-square 0 :sub-osc-sin 16 -10)))
        ;; sub-osc octave selector
        (AdvancedControl. 290 106 :selector {:caption   "OCTAVE"
                                             :ui-aux-fn (fn [] (text "-1" 315 119)
                                                               (text "-2" 315 135))}
                          :sub-osc-oct
                          (fn [val] (let [old-oct   (:sub-osc-oct @synth-state)
                                         new-state (alter-state
                                                    #(assoc % :sub-osc-oct
                                                            (if (zero? val)
                                                              ;; button press; switch to next oct
                                                              (case old-oct
                                                                1 2
                                                                2 1)
                                                              ;; knob or slider; calculate waveform
                                                              ([1 2] (int (* (/ val 128.0) (count [1 2])))))))
                                         new-oct   (:sub-osc-oct new-state)]
                                     ;; Toggle sub-osc octave
                                     [[:sub-osc-coeff (case (:sub-osc-oct @synth-state)
                                                        1 0.5
                                                        2 0.25)]]))
                          (fn [val] (case (:sub-osc-oct @synth-state)
                                     1 0 2 16)))

        ;; Sub-octave amount
        (AdvancedControl. 280 265 :slider {} :sub-osc-amp
                          (fn [val] (let [new-state (alter-state #(assoc % :sub-osc-amp (/ val 127.0)))]
                                     [[(:sub-osc-waveform @synth-state) (/ val 127.0)]]))
                          (fn [val] (* 127.0 (:sub-osc-amp @synth-state))))

        ;; Mod wheel function
        (AdvancedControl. 283 335 :selector {:caption   "MOD Wheel"
                                             :ui-aux-fn (fn [] (text "Cutoff"  315 348)
                                                               (text "Vibrato" 315 358)
                                                               (text "LFOAmt"  315 368))}
                          :mod-wheel-fn
                          (fn [val] (let [old-fn   (:mod-wheel-fn @synth-state)
                                         new-state (alter-state
                                                   #(assoc % :mod-wheel-fn
                                                            (if (zero? val)
                                                              ;; button press; switch to next fn
                                                              (next-mod-wheel-fn old-fn)
                                                              ;; knob or slider; calc fn
                                                              (mod-wheel-fns (int (* (/ val 128.0) (count mod-wheel-fns)))))))
                                         new-fn    (:mod-wheel-fn new-state)]
                                     []))
                          (fn [val] (case  (:mod-wheel-fn @synth-state)
                                     :cutoff 0 :vibrato 10 :lfo-amount 20)))

        ;; Vibrato type selector
        (AdvancedControl. 424 338 :selector {:ui-aux-fn (fn [] (shape (state :trill-up-shape)   445 345)
                                                               (shape (state :sin-shape)        445 355)
                                                               (shape (state :trill-down-shape) 445 365))}
                          :vibrato-fn
                          (fn [val] (let [old-fn    (:vibrato-fn @synth-state)
                                          new-state (alter-state
                                                      #(assoc % :vibrato-fn
                                                              (if (zero? val)
                                                                ;; button press; switch to next fn
                                                                (next-vibrato-fn old-fn)
                                                                ;; knob or slider; calc fn
                                                                (vibrato-fns (int (* (/ val 128.0) (count vibrato-fns)))))))
                                          new-fn    (:vibrato-fn new-state)
                                          mod-wheel-pos (:mod-wheel-pos @synth-state)]
                                      (case (:vibrato-fn @synth-state)
                                        :vibrato [[:vibrato-amp (/ mod-wheel-pos 127.0)] [:vibrato-trill 0]]
                                        :trill-up [[:vibrato-amp 0.0] [:vibrato-trill (int (/ mod-wheel-pos 10))]]
                                        :trill-down [[:vibrato-amp 0.0] [:vibrato-trill (- (int (/ mod-wheel-pos 10)))]])))
                          (fn [val] (case (:vibrato-fn @synth-state)
                                      :vibrato 10
                                      :trill-up 0
                                      :trill-down 20)))

        ;; Ptch bend wheel
        (AdvancedControl. 100 165 :wheel {:caption "Pitch"} :pitch-wheel
                          ;; bend fn val:127->2.0 val:64->1.0
                          (fn [val] [[:bend (+ (* val (/ 1.0 63)) (- 2 (/ 127.0 63)))]])
                          (fn [val] val))
        ;; Mod-wheel
        (AdvancedControl. 170 165 :wheel {:caption "Modulation"} :mod-wheel
                          (fn [val] (alter-state #(assoc % :mod-wheel-pos val))
                                    (case (:mod-wheel-fn @synth-state)
                                     :cutoff [[:cutoff (* (- val 10) 100.0)]]
                                     :vibrato (case (:vibrato-fn @synth-state)
                                                :vibrato [[:vibrato-amp (/ val 127.0)] [:vibrato-trill 0]]
                                                :trill-up [[:vibrato-amp 0.0] [:vibrato-trill (int (/ val 10))]]
                                                :trill-down [[:vibrato-amp 0.0] [:vibrato-trill (- (int (/ val 10)))]])
                                     :lfo-amount (do
                                                   (alter-state #(assoc % :lfo-amp (/ val 127.0)))
                                                   [[(:lfo-waveform @synth-state) (/ val 127.0)]])))
                          (fn [val] val))

        ;; Arp mode selector
        (AdvancedControl. 751 398 :knob {:caption   "Mode"
                                         :ui-aux-fn #(do
                                                       (text-align :left)
                                                       (doall
                                                         (map-indexed
                                                           (fn [i e](apply text e (selector-knob-label-pos 752 400 i)))
                                                           ["Off" "Up" "Down" "Up/Dwn" "Rand"]))
                                                       (text-align :center))}
                          :arp-mode
                          (fn [val] (let [old-mode    (:arp-mode @synth-state)
                                          new-state   (alter-state
                                                       #(assoc % :arp-mode
                                                               (if (= val 0)
                                                                 ;; button press; switch to next mode
                                                                 (mod (inc old-mode) 5)
                                                                 ;; knob or slider; calculate mote
                                                                 (int (* (/ (inc val) 129.0) 5)))))
                                         new-mode (:arp-mode new-state)]
                                     [[:arp-mode new-mode]]))
                          (fn [val] (case (:arp-mode @synth-state)
                                     0 62 1 75 2 85 3 98 4 109)))

        ;; Arp range selector
        (AdvancedControl. 681 398 :knob {:caption   "Octave"
                                         :ui-aux-fn #(doall
                                                       (map-indexed
                                                         (fn [i e](apply text e (selector-knob-label-pos 686 403 i)))
                                                         ["1" "2" "3" "4"]))}
                          :arp-range
                          (fn [val] (let [old-range (:arp-range @synth-state)
                                          new-state (alter-state
                                                      #(assoc % :arp-range
                                                              (if (= val 0)
                                                                ;; button press; switch to next range
                                                                (inc (mod old-range 4))
                                                                ;; knob or slider; calculate range
                                                                (inc (int (* (/ (inc val) 129.0) 4))))))
                                         new-range  (:arp-range new-state)]
                                     [[:arp-range new-range]]))
                          (fn [val] (case (:arp-range @synth-state)
                                     1 60 2 75 3 88 4 101)))

        ]))

(def ctl->control (into {} (map (fn [e] {(:name e) e}) controls)))

(def ui-keys
  [;; black keys
   {:color :black :coords [101 472 136 700] :note :C#3}
   {:color :black :coords [172 472 201 700] :note :D#3}

   {:color :black :coords [282 472 316 700] :note :F#3}
   {:color :black :coords [349 472 382 700] :note :G#3}
   {:color :black :coords [416 472 452 700] :note :A#3}

   {:color :black :coords [524 472 559 700] :note :C#4}
   {:color :black :coords [596 472 634 700] :note :D#4}

   {:color :black :coords [704 472 744 700] :note :F#4}
   {:color :black :coords [774 472 806 700] :note :G#4}
   {:color :black :coords [841 472 875 700] :note :A#4}

   ;; white keys
   {:color :white :coords [ 65 472 124 824] :note :C3}
   {:color :white :coords [124 472 184 824] :note :D3}
   {:color :white :coords [184 472 246 824] :note :E3}
   {:color :white :coords [246 472 303 824] :note :F3}
   {:color :white :coords [303 472 366 824] :note :G3}
   {:color :white :coords [366 472 430 824] :note :A3}
   {:color :white :coords [430 472 490 824] :note :B3}
   {:color :white :coords [490 472 549 824] :note :C4}
   {:color :white :coords [549 472 610 824] :note :D4}
   {:color :white :coords [610 472 669 824] :note :E4}
   {:color :white :coords [669 472 728 824] :note :F4}
   {:color :white :coords [728 472 791 824] :note :G4}
   {:color :white :coords [791 472 852 824] :note :A4}
   {:color :white :coords [852 472 910 824] :note :B4}
   {:color :white :coords [910 472 972 824] :note :C5}])

(defn apply-control-map-from-file []
  (let [extFilter   (FileNameExtensionFilter. "Bindings (*.ctl)" (into-array  ["ctl"]))
        filechooser (doto (JFileChooser. "./presets")
                      (.setFileFilter extFilter))
        retval      (.showOpenDialog filechooser nil)]
    (if (= retval JFileChooser/APPROVE_OPTION)
      (let [path    (-> filechooser .getSelectedFile .getPath)
            ctl-map (-> path slurp read-string)]
        (reset!  dev-chan-note-cmd->control
                 (into {} (map (fn [[k v]] [k (ctl->control v)]) ctl-map)))))))

(defn save-control-map-to-file []
  (let [extFilter   (FileNameExtensionFilter. "Bindings (*.ctl)" (into-array  ["ctl"]))
        filechooser (doto (JFileChooser. "./presets")
                      (.setFileFilter extFilter)
                      (.setSelectedFile (File. "Untitled.ctl")))
        retval      (.showSaveDialog filechooser nil)]
    (if (= retval JFileChooser/APPROVE_OPTION)
      (let [path    (-> filechooser .getSelectedFile .getPath)
            out-obj (into {} (map (fn [[k v]] [k (:ctl v)]) @dev-chan-note-cmd->control))]
        (->> out-obj pr-str (spit path))))))

(defn load-synth-settings-from-file [path]
  (let [patch (-> path slurp read-string)]
    ;; reset the ui and synth to pre-file-loaded values
    (reset! ui-state {})
    (reset-synth-defaults mbsynth)
    (doall (map (fn [[k v]]
                  (println "Setting " k)
                  (let [control   (ctl->control k)
                        synth-val ((:synth-fn control) v)]
                    (ctl-ui-and-synth synth-val (:name control) v))) patch))
    (ctl-ui-and-synth :gate 0.0 nil nil)))

(defn apply-synth-settings-from-file []
  (let [extFilter   (FileNameExtensionFilter. "Patch (*.patch)" (into-array  ["patch"]))
        filechooser (doto (JFileChooser. "./presets")
                      (.setFileFilter extFilter))
        retval      (.showOpenDialog filechooser nil)]
    (if (= retval JFileChooser/APPROVE_OPTION)
      (let [path (-> filechooser .getSelectedFile .getPath)]
        (load-synth-settings-from-file path)))))

(defn save-synth-settings-to-file []
  (let [extFilter   (FileNameExtensionFilter. "Patch (*.patch)" (into-array  ["patch"]))
        filechooser (doto (JFileChooser. "./presets")
                      (.setFileFilter extFilter)
                      (.setSelectedFile (File. "Untitled.patch")))
        retval      (.showSaveDialog filechooser nil)]
    (if (= retval JFileChooser/APPROVE_OPTION)
      (let [path (-> filechooser .getSelectedFile .getPath)]
        (->> @ui-state pr-str (spit path))))))

(defn menuitem-selected [event]
  (case (.getActionCommand event)
    "Save Bindings" (save-control-map-to-file)
    "Open Bindings" (apply-control-map-from-file)
    "Save Patch"    (save-synth-settings-to-file)
    "Open Patch"    (apply-synth-settings-from-file)))

(defn setup []
  (smooth)
  (frame-rate 10)
  (background 0)
  ;; load the most bad-est preset possible!
  (load-synth-settings-from-file "./presets/way-huge.patch")

  (ctl (map :id synths) :gate 0)
  (set-state! :background-img              (load-image "background.png")
              :knob-img                    (load-image "knob.png")
              :knob-background-img         (load-image "knob-background.png")
              :slider-img                  (load-image "slider.png")
              :slider-background-img       (load-image "slider-background.png")
              :selector-img                (load-image "selector.png")
              :selector-background-img     (load-image "selector-background.png")
              :wheel-img                   (load-image "wheel.png")
              :wheel-dimple-img            (load-image "wheel-dimple.png")
              :wheel-dimple-inv-img        (load-image "wheel-dimple-inv.png")
              :wheel-dimple-background-img (load-image "wheel-dimple-background.png")
              :white-key-img               (load-image "white-key.png")
              :black-key-img               (load-image "black-key.png")
              :led-img                     (load-image "led.png")
              :led-background-img          (load-image "led-background.png")
              :overtone-circle-img         (load-image "overtone-circle.png")
              :overtone-text-img           (load-image "overtone-text.png")
              :mini-beast-text-img         (load-image "mini-beast-text.png")
              :random-slew-shape           (load-shape "random-slew.svg")
              :random-shape                (load-shape "random.svg")
              :sin-shape                   (load-shape "sin.svg")
              :square-shape                (load-shape "square.svg")
              :tri-shape                   (load-shape "tri.svg")
              :saw-shape                   (load-shape "saw.svg")
              :trill-down-shape            (load-shape "trill-down.svg")
              :trill-up-shape              (load-shape "trill-up.svg")))

(defn draw []
  (let [background-img      (state :background-img)
        led-background-img  (state :led-background-img)
        led-img             (state :led-img)
        overtone-circle-img (state :overtone-circle-img)
        overtone-text-img   (state :overtone-text-img)
        mini-beast-text-img (state :mini-beast-text-img)
        overtone-tint       (color 253 0 147)]
    (set-image 0 0 background-img)
    (tint overtone-tint)
    (image overtone-circle-img 65 20)
    (tint 255 255 255)
    (image overtone-text-img 65 20)
    (image mini-beast-text-img 125 50)
    (doall (map draw-control controls))
    (let [lfo         @(-> synths (nth 0) :taps :lfo)
          lfo-tint    (color 255 0 0 (* 255 lfo))
          amp         (apply max (map (fn [s] @(-> s :taps :amp-adsr)) synths))
          amp-tint    (color 0 255 0 (* 255 amp))
          fil         (apply max (map (fn [s] @(-> s :taps :filter-adsr)) synths))
          filter-tint (color 0 255 0 (* 255 fil))
          arp         @(-> synths (nth 0) :taps :arp)
          arp-tint    (color 255 0 0 (* 255 arp))
          off-tint    (color 65 65 65 255)
          draw-led    (fn [x y t]
                        (tint off-tint)
                        (image led-background-img (+ 10 x) (+ 10 y))
                        (tint t)
                        (image led-img x y))]

      (doall (map (partial apply draw-led)
                  [[590 388 lfo-tint]
                   [705 170 filter-tint]
                   [910 170 amp-tint]
                   [898 388 arp-tint]]))
      (tint (color 255 255 255 255))
      (doall (map (fn [k] (draw-key (first (:coords k))
                                    (second (:coords k))
                                    (:color k)
                                    (some (fn [v] (= (:note v) (note (:note k)))) @voices)))
                  (sort-by (fn [k] (case (:color k)
                                           :white 0
                                           :black 1)) ui-keys)))
      (tint (color 255 255 255 255)))))

(defn in-box?
  "is point [x y] inside the box bounded by [u v] [s t]?
  [u v]
    +---------+
    |         |
    | + [x y] |
    |         |
    +---------+ 
             [s t]"
  [x y u v s t]
  (and (> x u)
       (> y v)
       (< x s)
       (< y t)))



(defn key-note-at-xy
  "Return the note of the key that occupies the point at (x y).
  Nil if no key occupies the space."
  [x y]
  (if-let [k (first (filter (fn [k] (apply in-box? x y (:coords k))) ui-keys))]
    (note (:note k))
    nil))

(defn control-at-xy
  "Return the first control that occupies the point at (x y).
  Nil if no control occupies the space."
  [x y]
  (first (filter (fn [c] (case (:type c)
                           :knob     (< (dist x y (+ (:x c) 25) (+ (:y c) 25)) 30)
                           :slider   (in-box? x y (:x c) (- (:y c) 70) (+ (:x c) 26) (+ (:y c) 31))
                           :selector (in-box? x y (:x c) (:y c) (+ (:x c) 14) (+ (:y c) 32))
                           :wheel    (in-box? x y (:x c) (:y c) (+ (:x c) 25) (+ (:y c) 137))))
                 controls)))

(defn mouse-clicked []
  ;; toggle selected-control on mouse click
  (if (nil? @selected-control)
    (let [x (mouse-x)
          y (mouse-y)
          c (control-at-xy x y) ]
      (println "click at [" x ", " y "]")
      (reset! selected-control c))
    (reset! selected-control nil)))

(defn mouse-dragged []
  "For dragging controls around using mouse"
  (let [x            (mouse-x)
        y            (mouse-y)]
    (when-let [c (if (nil? @dragged-control)
                   (reset! dragged-control (control-at-xy x y))
                   @dragged-control)]
        ;; move sliders 1-to-1 with the ui (* 1/0.6)
        ;; move selectors at an increased rate (2x)
      (let [dy           (* (case (:type c)
                              :slider (/ 1.0 0.6)
                              :selector 2.0
                              1.0)
                            (- y (pmouse-y)))
            control-name (:name c)
            last-val     (or (get @ui-state control-name) 0)
            ;; constrain new-val to 0-127.0
            new-val      (constrain (- last-val dy) 0.0 127.0)
            synth-ctls   ((:synth-fn c) new-val)
            ui-val       ((:ui-fn c) new-val)]
        (println "last-val " last-val " dy " dy " new-val " new-val " ctls " synth-ctls " ui-val " ui-val)
        (ctl-ui-and-synth synth-ctls control-name ui-val)))))

;; keyboard key press using mouse
(defn mouse-pressed []
  (when-let [note (key-note-at-xy (mouse-x) (mouse-y))]
    (println "Playing " (find-note-name note))
    (reset! mouse-pressed-note note)
    (keydown note 1.0)))

;; stop dragging & keyup if over key
(defn mouse-released []
  (reset! dragged-control nil)
  (when-let [note @mouse-pressed-note]
    (keyup note)))

(defn key-code->note [key-code]
  (get ;; Row one
   {\q (note :C3)
    \2 (note :C#3)
    \w (note :D3)
    \3 (note :D#3)
    \e (note :E3)
    \r (note :F3)
    \5 (note :F#3)
    \t (note :G3)
    \6 (note :G#3)
    \y (note :A3)
    \7 (note :A#3)
    \u (note :B3)
    \i (note :C4)
    \9 (note :C#4)
    \o (note :D4)
    \0 (note :D#4)
    \p (note :E4)
    ;; Row two
    \z (note :C2)
    \s (note :C#2)
    \x (note :D2)
    \d (note :D#2)
    \c (note :E2)
    \v (note :F2)
    \g (note :F#2)
    \b (note :G2)
    \h (note :G#2)
    \n (note :A2)
    \j (note :A#2)
    \m (note :B2)
    \, (note :C3)
    \l (note :C#3)
    \. (note :D3)
    \; (note :D#3)
    \/ (note :E3)} key-code))

(defn key-pressed []
  (when-let [note (key-code->note (raw-key))]
    (println "Playing " (find-note-name note))
    (keydown note 1.0)))

(defn key-released []
  (when-let [note (key-code->note (raw-key))]
    (keyup note)))


(defn register-midi-handlers
  []
  (on-sync-event [:midi :note-on]
                 (fn [{note :note velocity :velocity}]
                   (keydown note (/ velocity 127.0)))
                 ::note-on-handler)

  (on-sync-event [:midi :note-off]
                 (fn [{note :note}]
                   (keyup note))
                 ::note-off-handler)

  (on-sync-event [:midi :control-change]
                 handle-control
                 ::ctl-event-handler)

  (on-sync-event [:midi :pitch-bend]
                 handle-control
                 ::bend-event-handler))

(defn start-gui
  []
  (let [listener (proxy [ActionListener] []
                   (actionPerformed [event]
                     (menuitem-selected event)))
        mb       (doto (JMenuBar.)
                   (.add (doto (JMenu. "File")
                           (.add (doto (JMenuItem. "Open Bindings")
                                   (.addActionListener listener)))
                           (.add (doto (JMenuItem. "Save Bindings")
                                   (.addActionListener listener)))
                           (.add (doto (JMenuItem. "Open Patch")
                                   (.addActionListener listener)))
                           (.add (doto (JMenuItem. "Save Patch")
                                   (.addActionListener listener))))))
        sk       (sketch
                  :title "MiniBeast"
                  :setup setup
                  :draw draw
                  :mouse-clicked mouse-clicked
                  :mouse-dragged mouse-dragged
                  :mouse-pressed mouse-pressed
                  :mouse-released mouse-released
                  :key-pressed key-pressed
                  :key-released key-released
                  :decor true
                  :size [1036 850])
        frame    (-> sk meta :target-obj deref)]
    (doto frame
      (.setJMenuBar mb)
      (.setVisible true))))

(defn -main
  [& args]
  (println "--> Starting The MiniBeast...")
  (register-midi-handlers)
  (start-gui)
  (println "--> Beast started..."))
