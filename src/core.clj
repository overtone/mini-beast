(ns minibeast.core
  (:import [javax.swing JFileChooser JMenuBar JMenu JMenuItem]
           [javax.swing.filechooser FileNameExtensionFilter]
           [java.awt.event ActionListener]
           [java.io File]
           [java.awt.Toolkit])
  (:use [overtone.live :exclude (mouse-button mouse-x mouse-y rotate fill)]
        [overtone.helpers.system :only [mac-os?]]
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

(def main-g (group))
(def arp-g (group :head main-g))
(def lfo-g (group :after arp-g))
(def voice-g (group :after lfo-g))
(def synth-g (group :after voice-g))

(def lfo-bus-a (control-bus))
(def arp-trig-bus-a (control-bus))
(def arp-note-bus-a (control-bus))
(def voice-bus-a (audio-bus))

(def lfo-bus-b (control-bus))
(def arp-trig-bus-b (control-bus))
(def arp-note-bus-b (control-bus))
(def voice-bus-b (audio-bus))

(def arp-synth-a (darp [:tail arp-g] arp-trig-bus-a arp-note-bus-a))
(def arp-synth-b (darp [:tail arp-g] arp-trig-bus-b arp-note-bus-b))

(def lfo-synth-a (LFO [:tail lfo-g] lfo-bus-a arp-trig-bus-a))
(def lfo-synth-b (LFO [:tail lfo-g] lfo-bus-b arp-trig-bus-b))

(def synth-voices-a
  (doall (map (fn [_] (voice [:head voice-g]
                             voice-bus-a
                             lfo-bus-a
                             arp-trig-bus-a
                             arp-note-bus-a))
              (range 4))))
(def synth-voices-b
  (doall (map (fn [_] (voice [:head voice-g] 
                             voice-bus-b
                             lfo-bus-b
                             arp-trig-bus-b
                             arp-note-bus-b))
              (range 4))))

(def mb-synth-a (mbsynth [:head synth-g] voice-bus-a))
(def mb-synth-b (mbsynth [:head synth-g] voice-bus-b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal synth state
;;
;; These definitions store and maniupulate state internal
;; to the operation of the state of the synth.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord SynthState
  [sub-osc-waveform
   sub-osc-amp
   sub-osc-oct
   octave-transpose
   lfo-waveform
   lfo-amp
   lfo-arp-sync
   filter-type
   mod-wheel-fn
   mod-wheel-pos
   vibrato-fn
   bend-range
   env-speed
   arp-mode
   arp-range
   arp-step
   arp-tap-time])

(def synth-state (ref (SynthState. :sub-osc-square 0.0 1 0 :lfo-sin 1.0 0 :low-pass :cutoff 0.0 :vibrato 12 :fast 0 2 0 0)))

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

(defonce ui-state                   (atom {:a {} :b {}}))
(defonce selected-control           (atom nil))
(defonce dragged-control            (atom nil))
(defonce dev-chan-note-cmd->control (atom {}))
(defonce mouse-pressed-note         (atom nil))
(defonce show-modulation-controls?  (atom false))
(defonce selected-split             (atom :a))
(defonce split-note                 (atom 60))
(defonce first-draw?                (atom true))
(defonce composite-background-img   (atom nil))
(defonce control-key-pressed        (atom false))

(defn arp-synth []
  (case @selected-split
    :a arp-synth-a
    :b arp-synth-b))

(defn lfo-synth []
  (case @selected-split
    :a lfo-synth-a
    :b lfo-synth-b))

(defn synth-voices []
  (case @selected-split
    :a synth-voices-a
    :b synth-voices-b))

(defn mb-synth []
  (case @selected-split
    :a mb-synth-a
    :b mb-synth-b))

;; Keep a record of the last eight key presses
;; in a map of {:synth s, :note n}
(def voices-max 4)
(defonce voices-a (ref ()))
(defonce voices-b (ref ()))

(defn update-ui-state [m]
  (println "update-ui-state " m)
  (swap! ui-state (partial merge-with merge) {@selected-split m}))

(defn update-control
  "Control ui and synth parameters"
    [control new-val]
    ;; find the synth parameter this control controls
    (let [control-name (:name control)
          _                (update-ui-state {control-name new-val})
          synth-ctl-vals   ((:synth-fn control) new-val)
          _            (doall (map (fn [synth-ctl-val]
                           (let [synths       ((first synth-ctl-val))
                                 synth-ctl    (second synth-ctl-val)
                                 synth-val    (last synth-ctl-val)]
                               (ctl synths synth-ctl synth-val))) synth-ctl-vals))]))

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

(def filter-types [:low-pass :band-pass :high-pass :notch])
(defn next-filter-type [m]
  "Given a filter, return the next filter that can be selected."
  (let [modes (cycle filter-types)]
    (nth modes (inc (.indexOf modes m)))))


(def mod-wheel-fns [:cutoff :vibrato :lfo-amount])
(defn next-mod-wheel-fn [f]
  "Given mod wheel fn, return the next fn that can be selected."
  (let [fns (cycle mod-wheel-fns)]
    (nth fns (inc (.indexOf fns f)))))

(def vibrato-fns [:trill-up :vibrato :trill-down])
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

(defn getsynth [note voices]
  "Find a free synth or reclaim an old one, add the note and synth to
  the voices list and return the synth that was free or reclaimed."
  (dosync
   (if (>= (count @voices) voices-max)
     (let [voice     (dequeue voices)
           new-voice (assoc voice :note note)]
       (queue voices new-voice)
       (:synth new-voice))
     (let [synth-voices       (if (< note @split-note) synth-voices-a synth-voices-b)
           first-unused-synth (first (difference (set synth-voices)
                                                 (map (fn [e] (:synth e)) @voices)))
           new-voice          {:synth first-unused-synth :note note}]
       (queue voices new-voice)
       (:synth new-voice)))))


;; Find a synth and turn it on. Turn off an old synth if we need to free one up.
(defn keydown [note velocity]
  (dosync
    (let [voices (if (< note @split-note) voices-a voices-b)]
      (if-not (some #(= (:note %) note) @voices)
        (let [synth (getsynth note voices)]
          (println "playing " note  " with synth " (:id synth))
          ;; turn off the old note. Maybe this is a reused synth
          (ctl synth :gate 0.0)
          ;; turn on the new note
          (ctl synth :note note)
          (ctl synth :velocity velocity)
          (ctl synth :gate 1.0))))))

;; Find a synth to turn off
(defn keyup [note]
  (let [split (if (< note @split-note) :a :b)
        voices (case split :a voices-a :b voices-b)]
    (if-let [voice (remove-first-match #(= (:note %) note) voices)]
      (ctl (:synth voice) :gate 0.0))))

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
      (update-control matched-control vel))
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
;; synths a list of synths to control
;; ctl  specifier of which synth parameter to control
;; f function to transform midi velocity value to synth param value
(defrecord Control [x y type ui-hints synths ctl f])

(defrecord AdvancedControl [x y type ui-hints name synth-fn ui-fn])

(defmacro do-transformation
  "Saves current transformation, performs body, restores current
  transformation on exit."
  [& body]
  `(do
     (push-matrix)
     ~@body
     (pop-matrix)))

(defmacro let-transformation
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

(defn draw-knob [x y amount selected? draw-foreground? draw-background?
                 pos-indicator? start-sym end-sym sym-dx sym-dy zero?
                 caption caption-dx caption-dy]
  (let-transformation
      [w2                  (/ 50 2.0)
       tl                  [(+ x w2) (+ y w2)]
       knob-background-img (state :knob-background-img)
       knob-img            (state :knob-img)]
    (apply translate tl)
    (when draw-background?
      (text-size 8)
      (text-align :center)
      (when pos-indicator?
        (image knob-background-img (- 0 w2) (- 1 w2)))
      (when-not (nil? start-sym)
        ;; draw start sym
        (text start-sym (- -23 (or sym-dx 0)) (+ 27 (or sym-dy 0))))
      (when-not (nil? end-sym)
        ;; draw end sym
        (text end-sym (+ 24 (or sym-dx 0)) (+ 27 (or sym-dy 0))))
      (when zero?
        ;; draw zero
        (text "0" 0 -27))
      (when-not (nil? caption)
        (let [cdx (or caption-dx 0)
              cdy (or caption-dy 0)]
          ;; draw caption
          (text caption (+ cdx 0) (+ cdy 30)))))
    (when draw-foreground?
      (rotate (- (* amount 0.038) 2.4))
      (translate (- w2) (- w2))
      (when selected?
        (apply tint selected-tint))
      (image knob-img 0 0)
      (tint 255 255 255))))

(defn draw-button [x y amount selected? draw-foreground? draw-background? caption caption-dx caption-dy]
  "x: x position
   y: y position
   amount: not used"
  (let [button-img (state :button-img)]
    (when draw-background?
      (when-not (nil? caption)
        (let [cdx (or caption-dx 0)
              cdy (or caption-dy 0)]
          (text-size 8)
          (text-align :center)
          (text caption (+ 22 cdx x) (+ cdy y 46)))))
    (when draw-foreground?
      (when selected?
        (apply tint selected-tint))
      (image button-img x y)
      (tint 255 255 255))))

(defn draw-small-button [x y amount selected? draw-foreground? draw-background? caption caption-dx caption-dy]
  "x: x position
   y: y position
   amount: not used"
  (let [button-img (state :small-button-img)]
    (when draw-background?
      (when-not (nil? caption)
        (let [cdx (or caption-dx 0)
              cdy (or caption-dy 0)]
          (text-size 8)
          (text-align :center)
          (text caption (+ 22 cdx x) (+ cdy y 30)))))
    (when draw-foreground?
      (when selected?
        (apply tint selected-tint))
      (image button-img x y)
      (tint 255 255 255))))

(defn draw-slider [x y amount selected? draw-foreground? draw-background? caption caption-dx caption-dy]
  "x: x position
     y: y position
     amount: 0.0-127.0"
  (let [slider-background-img (state :slider-background-img)
        slider-img            (state :slider-img)]
    (when draw-background?
      (image slider-background-img (- x 5) (- y 77))
      (when-not (nil? caption)
        (let [cdx (or caption-dx 0)
              cdy (or caption-dy 0)]
          (text-size 8)
          (text-align :center)
          (text caption (+ 16 cdx x) (+ cdy y 40)))))
    (when draw-foreground?
      (when selected?
        (apply tint selected-tint))
      (image slider-img x (+ y (* -0.6  amount)))
      (tint 255 255 255))))

(defn draw-selector [x y pos selected? draw-foreground? draw-background?
                     caption caption-dx caption-dy]
  "x: x position
     y: y position
     pos: y position offet"
  (let [selector-background-img (state :selector-background-img)
        selector-img            (state :selector-img) ]
    (when draw-background?
      (image selector-background-img (+ x 1) (+ y 3))
      (when-not (nil? caption)
        (let [cdx (or caption-dx 0)
              cdy (or caption-dy 0)]
          (text-size 8)
          (text-align :center)
          (text caption (+ 10 cdx x) (+ y cdy 44)))))
    (when draw-foreground?
      (when selected?
        (apply tint selected-tint))
      (image selector-img x (+ y pos))
      (tint 255 255 255))))

(defn draw-wheel [x y amount selected? _ _ caption caption-dx caption-dy]
  "x: x position on screen
   y: y position on screen
   amount: 0.0-127.0
   selected?: draw control in selected mode"
  (let [wheel-dimple-background-img (state :wheel-dimple-background-img)
        wheel-img                   (state :wheel-img)
        wheel-dimple-img            (state :wheel-dimple-img)
        wheel-dimple-inv-img        (state :wheel-dimple-inv-img)]
    (when-not (nil? caption)
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

(defn draw-control [control draw-foreground? draw-background?]
  (let [selected? (= (:name @selected-control) (:name control))
        ui-val    (or ((:ui-fn control) (get (get @ui-state @selected-split) (:name control))) 0)
        args      (conj ((juxt :x :y #((:ui-fn %) ui-val)) control) selected? draw-foreground? draw-background?)
        ui-hints  (:ui-hints control)]
    (case (:type control)
      :knob         (apply draw-knob           (apply conj args
                                                           ((juxt :pos-indicator? :start-sym :end-sym :sym-dx :sym-dy
                                                                  :zero? :caption :caption-dx :caption-dy) ui-hints)))
      :button       (apply draw-button         (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :small-button (apply draw-small-button   (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :slider       (apply draw-slider         (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :selector     (apply draw-selector       (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints)))
      :wheel        (apply draw-wheel          (apply conj args ((juxt :caption :caption-dx :caption-dy) ui-hints))))
    (when-let [ui-aux-fn (-> control :ui-hints :ui-aux-fn )]
      (ui-aux-fn))))

(defn control->advanced-control [control]
  ;; AcvancedContol => [x y type ui-hints name synth-fn ui-fn]
  (AdvancedControl.
   (:x control)
   (:y control)
   (:type control)
   (:ui-hints control)
   (:ctl control)
   (fn [val] [[(:synths control) (:ctl control) ((:f control) val)]])
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


(defn osc-controls []
  [(Control.         479 35 :knob (mk-pos-only-knob "Tri fold")       synth-voices :tri-fold-thresh   (fn [val] (+ (/ val -127.0) 1)))
   (Control.         341 35 :knob (mk-pos-only-knob "Detune Amt")     synth-voices :saw-detune-amp    (fn [val] (/ val 127.0)))
   (Control.         407 38 :knob (mk-pos-only-knob "Pulse Width"
                                                    {:start-sym "50%"
                                                     :end-sym "90%"
                                                     :sym-dy -5})     synth-voices :osc-square-pw     (fn [val] (+ (/ val 255.0) 0.5)))
   (Control.         341 102 :knob (mk-pos-only-knob   "Detune Rate") synth-voices :saw-detune        (fn [val] (/ val 8.0)))
   (Control.         409 102 :knob (mk-plus-minus-knob "ENV Amt")     synth-voices :osc-square-pw-env (fn [val] (- (/ val 64.0) 1.0)))
   (Control.         479 102 :knob (mk-plus-minus-knob "ENV Amt")     synth-voices :tri-fold-env      (fn [val] (- (/ val 64.0) 1.0)))
   ;; sub-octave osc waveform selector
   (AdvancedControl. 290 46  :selector {:caption "WAVE"
                                         :ui-aux-fn (fn [] (shape (state :square-shape) 310 51)
                                                           (shape (state :sin-shape)    310 65))}
                     :sub-osc-waveform
                     (fn [val] (let [old-waveform (:sub-osc-waveform @synth-state)
                                    new-state     (alter-state
                                                    #(assoc % :sub-osc-waveform
                                                            (if (zero? val)
                                                              ;; button press; switch to next waveform
                                                              (next-sub-osc-waveform (:sub-osc-waveform %))
                                                              ;; knob or slider; calculate waveform
                                                              (sub-osc-waveforms (int (constrain (* 2.0 (/ val 127.0)) 0 1))))))
                                    new-waveform  (:sub-osc-waveform new-state)
                                    _             (update-ui-state {:sub-osc-waveform (case new-waveform
                                                                                       :sub-osc-square 1
                                                                                       :sub-osc-sin    127)})]
                                ;; Toggle sub-osc waveform
                                [[synth-voices old-waveform 0] [synth-voices new-waveform (:sub-osc-amp @synth-state)]]))
                     (fn [val] (case (:sub-osc-waveform @synth-state)
                                :sub-osc-square 0 :sub-osc-sin 16 -10)))
   ;; sub-osc octave selector
   (AdvancedControl. 290 106 :selector {:caption "OCTAVE"
                                        :ui-aux-fn (fn [] (text "-1" 315 119)
                                                          (text "-2" 315 135))}
                     :sub-osc-oct
                     (fn [val] (let [old-oct  (:sub-osc-oct @synth-state)
                                    new-state (alter-state
                                                #(assoc % :sub-osc-oct
                                                        (if (zero? val)
                                                          ;; button press; switch to next oct
                                                          (case old-oct
                                                            1 2
                                                            2 1)
                                                          ;; knob or slider; calculate waveform
                                                          ([1 2] (int (constrain (* 2.0 (/ val 127.0)) 0 1))))))
                                    new-oct   (:sub-osc-oct new-state)
                                    _         (update-ui-state {:sub-osc-oct (case new-oct
                                                                              1 1
                                                                              2 127)})]
                                ;; Toggle sub-osc octave
                                [[synth-voices :sub-osc-coeff (case (:sub-osc-oct @synth-state)
                                                                    1 0.5
                                                                    2 0.25)]]))
                     (fn [val] (case (:sub-osc-oct @synth-state)
                                1 0 2 16)))])


(defn filter-controls []
  [(Control.         545 35  :knob (mk-pos-only-knob "Cutoff")        synth-voices :cutoff          (fn [val] (* (- val 10) 100.0)))
   (Control.         613 35  :knob (mk-pos-only-knob "Resonance")     synth-voices :resonance       (fn [val] (/ val 127.0)))
   (Control.         545 102 :knob (mk-plus-minus-knob "ENV Amt")     synth-voices :cutoff-env      (fn [val] (* (- val 64.0) 200.0)))
   (Control.         613 102 :knob (mk-plus-minus-knob "KBD Tracking" {:start-sym "0%"
                                                                       :end-sym "200%"
                                                                       :sym-dx 3
                                                                       :sym-dy -5})
                                                                      synth-voices :cutoff-tracking (fn [val] (/ val 64.0)))
   ;; Filter type knob
   (AdvancedControl. 670 35  :knob {:caption   "Mode"
                                    :ui-aux-fn #(do
                                                  (text-align :left)
                                                  (doall
                                                    (map-indexed
                                                      (fn [i e] (apply text e (selector-knob-label-pos 670 38 i)))
                                                      ["LP" "BP" "HP" "Notch"]))
                                                  (text-align :center))}
                     :filter-type
                     (fn [val] (let [old-mode (:filter-type @synth-state)
                                    new-state (alter-state
                                                  #(assoc % :filter-type
                                                          (if (zero? val)
                                                            ;; button press; switch to next mode
                                                            (next-filter-type (:filter-type %))
                                                            ;; knob or slider; calculate mode
                                                            (filter-types (int (* (/ val 128.0) (count filter-types)))))))
                                    new-mode  (:filter-type new-state)
                                    _         (update-ui-state {:filter-type (case new-mode
                                                                              :low-pass  1
                                                                              :band-pass 33
                                                                              :high-pass 65
                                                                              :notch     127)})]
                                ;; Toggle filter mode
                                [[synth-voices :filter-type (.indexOf filter-types new-mode)]]))
                     (fn [val] (case (:filter-type @synth-state)
                                :low-pass 60 :band-pass 72 :high-pass 83 :notch 96)))
   ;; envelope speed selector
   (AdvancedControl. 690 103 :selector {:caption   "ENV Speed"
                                        :ui-aux-fn (fn [] (text "Fast" 716 114)
                                                          (text "Slow" 716 130))}
                     :env-speed
                     (fn [val] (let [old-speed (:env-speed @synth-state)
                                    new-state  (alter-state
                                                 #(assoc % :env-speed
                                                         (if (zero? val)
                                                           ;; button press; switch to next sped
                                                           (case old-speed
                                                             :fast :slow
                                                             :slow :fast)
                                                           ;; knob or slider; calculate speed
                                                           ([:fast :slow] (int (constrain (* 2.0 (/ val 127.0)) 0 1))))))
                                    new-speed  (:env-speed new-state)
                                    _          (update-ui-state {:env-speed (case new-speed
                                                                             :fast 1
                                                                             :slow 127)})]
                                ;; Toggle env-speed
                                [[synth-voices :env-speed (case (:env-speed @synth-state)
                                                            :fast 1
                                                            :slow 10)]]))
                     (fn [val] (case (:env-speed @synth-state)
                                :fast 0 :slow 16)))])

(defn osc-mix-controls []
  [(Control.         320 265 :slider {} synth-voices :osc-saw     (fn [val] (/ val 127.0)))
   (Control.         360 265 :slider {} synth-voices :osc-square  (fn [val] (/ val 127.0)))
   (Control.         400 265 :slider {} synth-voices :osc-tri     (fn [val] (/ val 127.0)))
   (Control.         440 265 :slider {} synth-voices :osc-noise   (fn [val] (/ val 127.0)))
   (Control.         480 265 :slider {} synth-voices :osc-audio-in(fn [val] (/ val 127.0)))
   ;; Sub-octave amount
   (AdvancedControl. 280 265 :slider {} :sub-osc-amp
                     (fn [val] (let [new-state (alter-state #(assoc % :sub-osc-amp (/ val 127.0)))]
                                [[synth-voices (:sub-osc-waveform @synth-state) (/ val 127.0)]]))
                     (fn [val] (* 127.0 (:sub-osc-amp @synth-state))))])

(defn filter-asdr-controls []
  [(Control. 565 265 :slider {:caption "Attack"}  synth-voices :filter-attack  (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 0.9) 12.0)))
   (Control. 605 265 :slider {:caption "Decay"}   synth-voices :filter-decay   (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
   (Control. 645 265 :slider {:caption "Sustain"} synth-voices :filter-sustain (fn [val] (/ val 127.0)))
   (Control. 685 265 :slider {:caption "Release"} synth-voices :filter-release (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))])


(defn amp-adsr-controls []
  [(Control. 770 265 :slider {:caption "Attack"}  synth-voices :amp-attack  (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 0.9) 12.0)))
   (Control. 810 265 :slider {:caption "Decay"}   synth-voices :amp-decay   (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))
   (Control. 850 265 :slider {:caption "Sustain"} synth-voices :amp-sustain (fn [val] (/ val 127.0)))
   (Control. 890 265 :slider {:caption "Release"} synth-voices :amp-release (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 12.0)))])

(defn misc-controls []
  [(Control. 338 398 :knob (mk-pos-only-knob "Glide")      synth-voices :portamento   (fn [val] (/ val 1270.0)))
   (Control. 268 398 :knob (mk-pos-only-knob "Bend Range") synth-voices :bend-range   (fn [val] (* 12.0 (/ val 127.0))))
   (Control. 408 398 :knob (mk-pos-only-knob "Rate")       synth-voices :vibrato-rate (fn [val] (/ val 8.0)))
   ;; Vibrato type selector
   (AdvancedControl. 424 338 :selector {:ui-aux-fn (fn [] (shape (state :trill-up-shape)   445 345)
                                                          (shape (state :sin-shape)        445 355)
                                                          (shape (state :trill-down-shape) 445 365))}
                     :vibrato-fn
                     (fn [val] (let [old-fn        (:vibrato-fn @synth-state)
                                     new-state     (alter-state
                                                     #(assoc % :vibrato-fn
                                                             (if (zero? val)
                                                               ;; button press; switch to next fn
                                                               (next-vibrato-fn old-fn)
                                                               ;; knob or slider; calc fn
                                                               (let [num-fns (count vibrato-fns)]
                                                                 (vibrato-fns (int (constrain (* num-fns (/ val 127.0))
                                                                                              0 (dec num-fns))))))))
                                     new-fn        (:vibrato-fn new-state)
                                     _             (update-ui-state {:vibrato-fn (case new-fn
                                                                                  :trill-up   1
                                                                                  :vibrato    70
                                                                                  :trill-down 127)})
                                     mod-wheel-pos (:mod-wheel-pos @synth-state)]
                                 (case (:vibrato-fn @synth-state)
                                   :vibrato    [[synth-voices :vibrato-amp (/ mod-wheel-pos 127.0)] 
                                                [synth-voices :vibrato-trill 0]]
                                   :trill-up   [[synth-voices :vibrato-amp 0.0]
                                                [synth-voices :vibrato-trill (int (/ mod-wheel-pos 10))]]
                                   :trill-down [[synth-voices :vibrato-amp 0.0]
                                                [synth-voices :vibrato-trill (- (int (/ mod-wheel-pos 10)))]])))
                     (fn [val] (case (:vibrato-fn @synth-state)
                                 :vibrato 10
                                 :trill-up 0
                                 :trill-down 20)))
   ;; Mod wheel function
   (AdvancedControl. 281 335 :selector {:caption   "MOD Wheel"
                                        :ui-aux-fn (fn [] (text "Cutoff"  311 348)
                                                          (text "Vibrato" 312 358)
                                                          (text "LFOAmt"  313 368))}
                     :mod-wheel-fn
                     (fn [val] (let [old-fn   (:mod-wheel-fn @synth-state)
                                    new-state (alter-state
                                              #(assoc % :mod-wheel-fn
                                                       (if (zero? val)
                                                         ;; button press; switch to next fn
                                                         (next-mod-wheel-fn old-fn)
                                                         ;; knob or slider; calc fn
                                                         (let [num-fns (count mod-wheel-fns)]
                                                           (mod-wheel-fns (int (constrain (* num-fns (/ val 127.0))
                                                                                          0 (dec num-fns))))))))
                                    new-fn    (:mod-wheel-fn new-state)
                                    _         (update-ui-state {:mod-wheel-fn (case new-fn
                                                                               :cutoff 1
                                                                               :vibrato 65
                                                                               :lfo-amount 127)})]
                                []))
                     (fn [val] (case  (:mod-wheel-fn @synth-state)
                                :cutoff 0 :vibrato 10 :lfo-amount 20)))
   ;; Show modulation panel
   (AdvancedControl. 64 434 :small-button {:caption "Toggle panel"}
                     :toggle-modulation-controls
                     (fn [val]
                       (if (zero? val)
                         (swap! show-modulation-controls? not)
                         (reset! show-modulation-controls? (> val 64.0)))
                       [])
                     (fn [val] 0))])

(defn lfo-contols []
  [(Control. 546 398 :knob (mk-pos-only-knob "Rate")              lfo-synth    :lfo-rate
             (fn [val] (/ (- (Math/pow 1.01 (* val 5.0)) 1.0) 3.0)))

   (Control. 475 332 :knob (mk-plus-minus-knob "PWM")             synth-voices :lfo2pwm    (fn [val] (- (/ val 32.0) 1.98)))
   (Control. 544 332 :knob (mk-plus-minus-knob "Pitch")           synth-voices :lfo2pitch  (fn [val] (- (/ val 2.0) 32.0)))
   (Control. 612 332 :knob (mk-plus-minus-knob "Filter")          synth-voices :lfo2filter (fn [val] (* (- val 64.0) 400.0)))
   (Control. 681 332 :knob (mk-plus-minus-knob "Amp"
                                                {:caption-dx -25
                                                 :caption-dy -55}) synth-voices :lfo2amp   (fn [val] (- (/ val 32) 1.98)))
   ;; LFO waveform knob
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
                                    new-state     (alter-state
                                                      #(assoc % :lfo-waveform
                                                          (if (zero? val)
                                                            ;; button press; switch to next waveform
                                                            (next-lfo-waveform (:lfo-waveform %))
                                                            ;; knob or slider; calculate waveform
                                                            (lfo-waveforms (int (* (/ val 128.0) (count lfo-waveforms)))))))
                                    new-waveform (:lfo-waveform new-state)
                                    _            (update-ui-state {:lfo-waveform (case new-waveform
                                                                                  :lfo-sin       1
                                                                                  :lfo-tri       23
                                                                                  :lfo-saw       45
                                                                                  :lfo-square    67
                                                                                  :lfo-rand      89
                                                                                  :lfo-slew-rand 127)})]
                                ;; Toggle lfo waveform
                                [[lfo-synth old-waveform 0] [lfo-synth new-waveform (:lfo-amp @synth-state)]]))
                     (fn [val] (case (:lfo-waveform @synth-state)
                                :lfo-sin 60 :lfo-tri 75 :lfo-saw 89 :lfo-square 100 :lfo-rand 115 :lfo-slew-rand 130)))
   ;; lfo-arp sync selector
   (AdvancedControl. 620 398 :selector {:caption   "Clock"
                                        :ui-aux-fn (fn [] (text "Free" 645 410)
                                                          (text "Sync" 645 426))}
                     :lfo-arp-sync
                     (fn [val] (let [last-val  (:lfo-arp-sync @synth-state)
                                     new-state (alter-state
                                                 #(assoc % :lfo-arp-sync
                                                         (if (zero? val)
                                                           ;; button press; switch to next val
                                                           (case last-val
                                                             0 1
                                                             1 0)
                                                           ;; knob or slider; calculate val
                                                           ([0 1] (int (constrain (* 2.0 (/ val 127.0)) 0 1))))))
                                    new-val   (:lfo-arp-sync new-state)
                                    _         (update-ui-state {:lfo-arp-sync (case new-val
                                                                               0 1
                                                                               1 127)})]
                                ;; Toggle flag
                                [[lfo-synth :lfo-arp-sync new-val]]))
                     (fn [val] (case (:lfo-arp-sync @synth-state)
                                0 0 1 16)))])

(defn arp-controls []
  [(Control. 885 332 :knob (mk-pos-only-knob "Tempo") arp-synth :arp-rate 
             (fn [val] (let [rate (/ val 5.0)]
                         (println "arp-rate " (* (/ 60 8) rate) " bmp")
                         rate)))
   (Control. 829 398 :knob (mk-pos-only-knob "Swing") arp-synth :arp-swing-phase (fn [val] (* 360 (/ val 127.0))))
   ;; Arp mode selector
   (AdvancedControl. 742 398 :knob {:caption   "Mode"
                                    :ui-aux-fn #(do
                                                  (text-align :left)
                                                  (doall
                                                    (map-indexed
                                                      (fn [i e](apply text e (selector-knob-label-pos 743 400 i)))
                                                      ["Off" "Up" "Down" "Up/Dwn" "Rand"]))
                                                  (text-align :center))}
                     :arp-mode
                     (fn [val] (let [old-mode  (:arp-mode @synth-state)
                                     new-state (alter-state
                                                #(assoc % :arp-mode
                                                        (if (zero? val)
                                                          ;; button press; switch to next mode
                                                          (mod (inc old-mode) 5)
                                                          ;; knob or slider; calculate mode
                                                          (int (constrain (* 5.0 (/ val 127.0)) 0 4)))))
                                     new-mode  (:arp-mode new-state)
                                     _         (update-ui-state {:arp-mode (case new-mode
                                                                            0 1
                                                                            1 30
                                                                            2 56
                                                                            3 82
                                                                            4 127)})
                                     _         (println "new-mode " new-mode)]
                                [[arp-synth :arp-mode new-mode]]))
                     (fn [val] (case (int (:arp-mode @synth-state))
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
                                                         (if (zero? val)
                                                           ;; button press; switch to next range
                                                           (inc (mod old-range 4))
                                                           ;; knob or slider; calculate range
                                                           (inc (int (* (/ (inc val) 129.0) 4))))))
                                    new-range  (:arp-range new-state)
                                    _          (update-ui-state {:arp-range (case new-range
                                                                             1 1
                                                                             2 33
                                                                             3 65
                                                                             4 127)})]
                                [[arp-synth :arp-range new-range]]))
                     (fn [val] (case (:arp-range @synth-state)
                                1 60 2 75 3 88 4 101)))

   ;; Arp step selector
   (AdvancedControl. 810 336 :knob {:caption   "Step"
                                    :ui-aux-fn #(doall
                                                  (map-indexed
                                                    (fn [i e](apply text e (selector-knob-label-pos 820 340 i)))
                                                    ["1/4" "1/8" "1/16" "1/4T" "1/8T" "1/16T"]))}
                     :arp-step
                     (fn [val] (let [old-step  (:arp-step @synth-state)
                                     new-state (alter-state
                                                 #(assoc % :arp-step
                                                         (if (zero? val)
                                                           ;; button press; switch to next step
                                                           (mod (inc old-step) 6)
                                                           ;; knob or slider; calculate range
                                                           (int (* (/ (inc val) 129.0) 6)))))
                                    new-step   (:arp-step new-state)
                                    _          (update-ui-state {:arp-step (case new-step
                                                                            0 1
                                                                            1 23
                                                                            2 45
                                                                            3 67
                                                                            4 89
                                                                            5 127)})]
                                [[arp-synth :arp-step new-step]]))
                     (fn [val] (case (:arp-step @synth-state)
                                0 65 1 75 2 84 3 98 4 111 5 125)))
   ;; Arp tap tempo button
   (AdvancedControl. 889 407 :button {:caption "Tap"} :arp-tap-tempo
                     (fn [val] 
                       (let [dt (- (now) (:arp-tap-time @synth-state))]
                         (alter-state #(assoc % :arp-tap-time (now)))
                         (if (< dt 2000)
                           [[arp-synth :arp-rate (/ 1000 dt)]]
                           [])))
                     (fn [val] val))])

(defn volume-controls []
  [(Control. 885 38 :knob (mk-pos-only-knob "Master Volume") mb-synth     :volume       (fn [val] (/ val 12.0)))
   (Control. 750 38 :knob (mk-pos-only-knob "Feedback Amt")  synth-voices :feedback-amp (fn [val] (/ val 120.0)))])

(defn wheel-controls []
   ;; Ptch bend wheel
  [(AdvancedControl. 100 165 :wheel {:caption "Pitch"} :pitch-wheel
                     ;; bend fn val:127->1.0 val:64->0.0 val:0->-1.0
                     (fn [val] [[synth-voices :bend (- (* 2.0 (/ val 127.0)) 1.0)]])
                     (fn [val] val))
   ;; Mod-wheel
   (AdvancedControl. 170 165 :wheel {:caption "Modulation"} :mod-wheel
                     (fn [val] (alter-state #(assoc % :mod-wheel-pos val))
                               (case (:mod-wheel-fn @synth-state)
                                :cutoff       [[synth-voices :cutoff (* (- val 10) 100.0)]]
                                :vibrato (case (:vibrato-fn @synth-state)
                                           :vibrato    [[synth-voices :vibrato-amp (/ val 127.0)]
                                                        [synth-voices :vibrato-trill 0]]
                                           :trill-up   [[synth-voices :vibrato-amp 0.0]
                                                        [synth-voices :vibrato-trill (int (/ val 10))]]
                                           :trill-down [[synth-voices :vibrato-amp 0.0]
                                                        [synth-voices :vibrato-trill (- (int (/ val 10)))]])
                                :lfo-amount (do
                                              (alter-state #(assoc % :lfo-amp (/ val 127.0)))
                                              [[lfo-synth (:lfo-waveform @synth-state) (/ val 127.0)]])))
                     (fn [val] val))])

(defn mod-controls []
  [(Control. 60  491 :knob (mk-pos-only-knob "Level")  mb-synth :delay-mix      (fn [val] (/ val 127.0)))
   (Control. 130 491 :knob (mk-pos-only-knob "F.Back") mb-synth :delay-feedback (fn [val] (/ val 127.0)))
   (Control. 200 491 :knob (mk-pos-only-knob "Time")   mb-synth :delay-time     (fn [val] (/ val 127.0)))

   (Control. 60  566 :knob (mk-pos-only-knob "Mix")    mb-synth :reverb-mix     (fn [val] (/ val 127.0)))
   (Control. 130 566 :knob (mk-pos-only-knob "Size")   mb-synth :reverb-size    (fn [val] (/ val 127.0)))
   (Control. 200 566 :knob (mk-pos-only-knob "Damp")   mb-synth :reverb-damp    (fn [val] (/ val 127.0)))
   
   (AdvancedControl. 70 652 :selector {:caption "Split Select"
                                       :ui-aux-fn (fn [] (text "Left"  96 664)
                                                         (text "Right" 96 680))}
                     :selected-split
                     (fn [val] (do
                                 (reset! selected-split
                                         (if (zero? val)
                                          ;; button press; switch to next mode
                                          (case @selected-split
                                            :a :b
                                            :b :a)
                                           ;; knob or slider; calculate mode
                                           (let [splits     [:a :b]
                                                 num-splits (count splits)]
                                             (splits (int (constrain (* num-splits (/ val 127.0)) 0 (dec num-splits)))))))
                                 []))
                     (fn [val] (case @selected-split
                                :a 0 :b 16)))
   (AdvancedControl. 120 642 :knob {:caption "Split Note"
                                    :pos-indicator? true
                                    :ui-aux-fn (fn [] (fill (color 0 0 0 255))
                                                      (rect 180 658 40 24)
                                                      (fill (color 255 0 0 255))
                                                      (text-size 16)
                                                      (text (str (find-note-name (note @split-note))) 199 676)
                                                      (text-size 8)
                                                      (fill (color 255 255 255 255)))}
                    :split-note
                    (fn [val] (do
                                (reset! split-note (int val))
                                []))
                    (fn [val] val))])

(defn octave-controls []
  [(AdvancedControl. 100 392 :small-button {:caption "Down"} :octave-down
                     (fn [val] (let [new-state (alter-state #(assoc % :octave-transpose (max -2 (dec (:octave-transpose %)))))]
                                 [[synth-voices :octave-transpose (:octave-transpose new-state)]]))
                     (fn [val] 0))
   (AdvancedControl. 175 392 :small-button {:caption "Up"}   :octave-up
                     (fn [val] (let [new-state (alter-state #(assoc % :octave-transpose (min  2 (inc (:octave-transpose %)))))]
                                 [[synth-voices :octave-transpose (:octave-transpose new-state)]]))
                     (fn [val] 0))])

(defn get-mod-controls []
  (let [get-mod-controls-helper
          (memoize (fn [] 
            (map (fn [c] (if (= (type c) Control)
                                (control->advanced-control c)
                                c)) (mod-controls))))]
    (get-mod-controls-helper)))

(defn get-controls 
  ([]
   (get-controls @show-modulation-controls?))
  ([show-modulation-controls?]
    (let [get-controls-helper
            (memoize
              (fn [show-mod-controls?]
                (map (fn [c] (if (= (type c) Control)
                                 (control->advanced-control c)
                                 c))
                     (concat (osc-controls)
                             (filter-controls)
                             (osc-mix-controls)
                             (filter-asdr-controls)
                             (amp-adsr-controls)
                             (misc-controls)
                             (lfo-contols)
                             (arp-controls)
                             (volume-controls)
                             (wheel-controls)
                             (octave-controls)
                         (if show-mod-controls?
                             (mod-controls)
                             [])))))]
      (get-controls-helper show-modulation-controls?))))

;; populate the map with ALL the controls.
(def ctl->control (into {} (map (fn [e] {(:name e) e}) (get-controls true))))

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
   {:color :white :coords [428 472 490 824] :note :B3}
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
    (swap! ui-state merge {@selected-split {}})
    (reset-synth-defaults mbsynth)
    (doall (map (fn [[k v]]
                  (println "Setting " k)
                  (let [control    (ctl->control k)]
                    ;; zero values are considered clicks. Just make them 1's instead.
                    (update-control control (max 1 v)))) patch))))

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
        (->> (get @ui-state @selected-split) pr-str (spit path))))))

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
  (reset! selected-split :b)
  (load-synth-settings-from-file "./presets/bass.patch")
  (reset! selected-split :a)
  (set-state! :background-img              (load-image "background.png")
              :mod-panel-img               (load-image "mod-panel.png")
              :button-img                  (load-image "button.png")
              :small-button-img            (load-image "small-button.png")
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
              :logo-img                    (load-image "logo.png")
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
        logo-img            (state :logo-img)
        overtone-tint       (color 253 0 147)]
    (if @first-draw?
      (let [draw-foreground? false
            draw-background? true
            tmp-background   "tmp-background.png"]
        (reset! first-draw? false)
        (println "--> Compositing background...")
        (set-image 0 0 background-img)
        (tint overtone-tint)
        (image overtone-circle-img 65 20)
        (tint 255 255 255)
        (image overtone-text-img 65 20)
        (image mini-beast-text-img 125 50)
        (image logo-img 120 80)
        (doall (map #(draw-control % draw-foreground? draw-background?) (get-controls false)))
        (save tmp-background)
        (println "--> Loading new background...")
        (reset! composite-background-img (load-image tmp-background))))

    (set-image 0 0 @composite-background-img)
    ;; draw all keys
    (doall (map (fn [k] (draw-key (first (:coords k))
                                  (second (:coords k))
                                  (:color k)
                                  (some (fn [v] (= (:note v) (note (:note k)))) (concat @voices-a @voices-b))))
                (sort-by (fn [k] (case (:color k)
                                         :white 0
                                         :black 1)) ui-keys)))
    (tint (color 255 255 255 255))

    ;; draw modulation panel
    (when @show-modulation-controls?
      ; draw background
      (image (state :mod-panel-img) 50 472)
      (doall (map #(draw-control % true true) (get-mod-controls))))

    ;; draw regular controls
    (let [draw-foreground? true
          draw-background? false]
      (doall (map #(draw-control % draw-foreground? draw-background?) (get-controls false))))

    (let [lfo              (or @(-> (lfo-synth) :taps :lfo) 0)
          lfo-tint         (color 255 0 0 (* 255 lfo))
          amp              (apply max 0 (map (fn [s] @(-> s :taps :amp-adsr)) (concat synth-voices-a synth-voices-b)))
          amp-tint         (color 0 255 0 (* 255 amp))
          fil              (apply max 0 (map (fn [s] @(-> s :taps :filter-adsr)) (concat synth-voices-a synth-voices-b)))
          filter-tint      (color 0 255 0 (* 255 fil))
          arp              (or @(-> (arp-synth) :taps :arp) 0)
          arp-tint         (color 255 0 0 (* 255 arp))
          off-tint         (color 65 65 65 255)
          down-2-oct-tint  (color 255 0   0 (if (= -2 (:octave-transpose @synth-state)) 255 66))
          down-1-oct-tint  (color 255 255 0 (if (= -1 (:octave-transpose @synth-state)) 255 66))
          down-0-oct-tint  (color 0   255 0 (if (=  0 (:octave-transpose @synth-state)) 255 66))
          up-1-oct-tint    (color 255 255 0 (if (=  1 (:octave-transpose @synth-state)) 255 66))
          up-2-oct-tint    (color 255 0   0 (if (=  2 (:octave-transpose @synth-state)) 255 66))
          draw-led    (fn [x y t]
                        (tint off-tint)
                        (image led-background-img (+ 10 x) (+ 10 y))
                        (tint t)
                        (image led-img x y))]

      (doall (map (partial apply draw-led)
                  [[590 388 lfo-tint]
                   [705 170 filter-tint]
                   [910 170 amp-tint]
                   [898 383 arp-tint]
                   [102 354 down-2-oct-tint]
                   [122 354 down-1-oct-tint]
                   [142 354 down-0-oct-tint]
                   [162 354 up-1-oct-tint]
                   [182 354 up-2-oct-tint]]))
      (tint (color 255 255 255 255))
      (doall (map (fn [t] (apply text ((juxt :t :x :y) t))) [{:x 114 :y 360 :t "-2"}
                                                             {:x 134 :y 360 :t "-1"}
                                                             {:x 156 :y 360 :t "0"}
                                                             {:x 174 :y 360 :t "+1"}
                                                             {:x 194 :y 360 :t "+2"}])))))

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
  (if-let [k (first (filter (fn [k] (and (apply in-box? x y (:coords k))
                                         (if @show-modulation-controls?
                                           (not (in-box? x y 49 470 263 711))
                                           true))) ui-keys))]
    (note (:note k))
    nil))

(defn control-at-xy
  "Return the first control that occupies the point at (x y).
  Nil if no control occupies the space."
  [x y]
  (first (filter (fn [c] (case (:type c)
                           :knob           (< (dist x y (+ (:x c) 25) (+ (:y c) 25)) 30)
                           :small-button   (in-box? x y (:x c) (:y c)  (+ (:x c) 43) (+ (:y c) 22))
                           :button         (in-box? x y (:x c) (:y c)  (+ (:x c) 45) (+ (:y c) 37))
                           :slider         (in-box? x y (:x c) (- (:y c) 70) (+ (:x c) 26) (+ (:y c) 31))
                           :selector       (in-box? x y (:x c) (:y c) (+ (:x c) 14) (+ (:y c) 32))
                           :wheel          (in-box? x y (:x c) (:y c) (+ (:x c) 35) (+ (:y c) 137))))
                 (get-controls))))

(defn mouse-clicked []
  ;; toggle selected-control on mouse click
  (let [button (if @control-key-pressed
                 :right
                 (mouse-button))]
    (println button)
    (println "ctrl? " @control-key-pressed)
    (case button
      :left (when-let [matched-control (control-at-xy (mouse-x) (mouse-y))]
              (update-control matched-control 0))
      :right (if (nil? @selected-control)
               (let [x (mouse-x)
                     y (mouse-y)
                     c (control-at-xy x y)]
                 (println "right click at [" x ", " y "]")
                 (println "found control " c)
                 (reset! selected-control c))
               (reset! selected-control nil))
      nil)))

(defn mouse-dragged []
  "For dragging controls around using mouse"
  (when (= (mouse-button) :left)
    (let [x (mouse-x)
          y (mouse-y)]
      (when-let [c (if (nil? @dragged-control)
                     (reset! dragged-control (control-at-xy x y))
                     @dragged-control)]
          ;; move sliders 1-to-1 with the ui (* 1/0.6)
          ;; move selectors at an increased rate (8x)
        (let [dy       (* (case (:type c)
                            :slider (/ 1.0 0.6)
                            :selector -8.0
                            1.0)
                          (- y (pmouse-y)))
              last-val (or (get (get @ui-state @selected-split) (:name c)) 0)
              ;; constrain new-val to 1.0-127.0
              ;; don't actually get to zero because it is reserved for button presses
              new-val  (constrain (- last-val dy) 1.0 127.0)]
          (println "last-val " last-val " new-val " new-val)
          (when (not-any? (:type c) [:button :small-button])
            (update-control c new-val)))))))

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
    (keydown note 1.0))
  (when (= java.awt.event.KeyEvent/VK_CONTROL (key-code))
    (println "control pressed")
    (reset! control-key-pressed true)))

(defn key-released []
  (when-let [note (key-code->note (raw-key))]
    (keyup note))
  (when (= java.awt.event.KeyEvent/VK_CONTROL (key-code))
    (println "control released")
    (reset! control-key-pressed false)))

(defn close []
  (println "--> Beast stopped...")
  (kill-server))

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

(defmacro when* [test & body]
  (when test
    ``(~~@body)))

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
                  :on-close close
                  :decor true
                  :size [1036 850])
        frame    (-> sk meta :target-obj deref)
        icon     (.createImage (java.awt.Toolkit/getDefaultToolkit) "data/icon.png")]
    (doto frame
      (.setJMenuBar mb)
      (.setVisible true))
    (when* (mac-os?)
      (import com.apple.eawt.Application)
      (try
        (.setDockIconImage (com.apple.eawt.Application/getApplication) icon)
      (catch Exception e
        false)))))

(defn -main
  [& args]
  (println "--> Starting The MiniBeast...")
  (register-midi-handlers)
  (start-gui)
  (println "--> Beast started..."))
