(ns minibeast.mbsynth
    (:use [overtone.live]))

(defsynth mbsynth 
  [note           60            ; midi note value
   volume         1.0           ; gain of the output
   velocity       1.0           ; gain for the current note
   osc-sin        0.0           ; osc sin amount
   osc-saw        1.0           ; osc saw amount
   osc-square     0.0           ; osc square amount
   osc-tri        0.0           ; osc triangle amount
   osc-noise      0.0           ; osc nouse amount
   sub-osc-sin    0.0           ; sin sub-oscillator
   sub-osc-square 0.0           ; square sub-oscillator
   sub-osc-coeff  0.5           ; 0.5 = -1 octave 0.25 = -2 octave
   osc-square-pw  0.5           ; square osc pulse width
   osc-square-pw-env 0.0        ; pw modulation by amp envelope
   lfo-rate       4.0           ; frequency of the LFO
   tri-fold-thresh 1.0          ; fold threshold for triangle osc
   tri-fold-env   0.0           ; fold threshold env modulation for triangle osc
   cutoff-env     0.0           ; cutoff envelope modulation
   cutoff-tracking 1.0         ; keyboard tracking amount for filter cutoff
   lfo-sin        1.0           ; LFO sin amount
   lfo-saw        0.0           ; LFO saw amount
   lfo-square     0.0           ; LFO square amount
   lfo-tri        0.0           ; LFO triange amount
   lfo-rand       0.0           ; LFO random amount
   lfo-slew-rand  0.0           ; LFO smoothed random amount
   filter-attack  0.0           ; Filter envelope attack
   filter-decay   0.0           ; Filter envelope decay
   filter-sustain 0.0           ; Filter envelope sustain
   filter-release 0.0           ; Filter envelope release
   amp-attack     0.1           ; Amp envelope attack
   amp-decay      0.2           ; Amp envelope decay
   amp-sustain    0.2           ; Amp envelope sustain
   amp-release    0.2           ; Amp envelope release
   lfo2pitch      0.0           ; LFO pitch modulation
   lfo2filter     0.0           ; LFO filter modulation
   lfo2amp        0.0           ; LFO amp modulation
   lfo2pwm        0.0           ; LFO PWM modulation
   vibrato-rate   0.0           ; vibrato rate Hz
   vibrato-amp    0.0           ; amount of vibrato modulation 0.0-1.0
   cutoff         1000.0        ; cutoff frequency of the VCF
   resonance      1.0           ; resonance of the VCF
   bend           1.0           ; note bend coeffecient
   peak           0.5           ; VCF peak control (resonance)
   saw-detune     1.0           ; phase offset of parallel saw oscs
   saw-detune-amp 1.0           ; amount of detuned saw osc in the mix
   portamento     0.0           ; rate to change to new note
   gate           1.0           ; another output gain?
   reverb-mix     0.5           ; wet-dry mix
   reverb-size    0.3           ; reverb room size
   reverb-damp    0.8           ; reverb dampening
   feedback-amp   0.0           ; feedback amount
   ]
  (let [AMP-ADSR        (tap :amp-adsr 10
                             (env-gen (adsr amp-attack amp-decay amp-sustain amp-release) gate))
        FILTER-ADSR     (tap :filter-adsr 10
                             (env-gen (adsr filter-attack filter-decay filter-sustain filter-release) gate))
        LFO             (tap :lfo 10
                            (+ (* lfo-sin 
                                   (sin-osc lfo-rate))
                               (* lfo-saw
                                   (lf-saw lfo-rate))
                               (* lfo-square
                                   (square lfo-rate))
                               (* lfo-tri
                                   (lf-tri lfo-rate))
                               (* lfo-rand
                                   (t-rand -1.0 1.0 (sin-osc lfo-rate)))
                               (* lfo-slew-rand
                                   (slew (t-rand -1.0 1.0 (sin-osc lfo-rate) 16.0 16.0)))))
        VIBRATO-LFO     (* vibrato-amp (sin-osc vibrato-rate))
        input-note-freq (midicps note)
        glide-rate      (/ input-note-freq portamento)
        note-freq       (slew (+ (* input-note-freq bend) (* lfo2pitch LFO)), glide-rate, glide-rate)
        sub-note-freq   (* note-freq sub-osc-coeff)
        TRI-FOLD-THRESH (max 0.01 (+ (* FILTER-ADSR tri-fold-env) tri-fold-thresh))
        VCO             (+ (* osc-sin (sin-osc note-freq))
                           (* osc-saw (+ (saw note-freq)
                                         (* saw-detune-amp
                                            (+ (saw (+ note-freq (* 1.1 saw-detune)))
                                               (saw (- note-freq saw-detune))
                                               (saw (+ note-freq (* 2.1 saw-detune)))
                                               (saw (- note-freq (* 2 saw-detune)))))))
                           (* osc-square (pulse note-freq (+ (* FILTER-ADSR osc-square-pw-env) osc-square-pw (* LFO lfo2pwm))))
                           (* osc-tri (/ 1.0 TRI-FOLD-THRESH) (fold2 (lf-tri note-freq) TRI-FOLD-THRESH))
                           (* osc-noise (white-noise))
                           (* sub-osc-sin (sin-osc sub-note-freq))
                           (* sub-osc-square (square sub-note-freq (* LFO lfo2pwm))))
        vcf-freq        (+ cutoff (* lfo2filter LFO) (* cutoff-tracking note-freq) (* cutoff-env AMP-ADSR))
        VCF             (moog-ff (+ VCO (* feedback-amp (local-in))) (* FILTER-ADSR vcf-freq) resonance)
        VCA             (+ (* lfo2amp LFO) AMP-ADSR VIBRATO-LFO)
        OUT             (free-verb (comb-n (softclip (* volume velocity VCA VCF))
                                1.0
                                0.1
                                0.5) reverb-mix reverb-size reverb-damp)
        _               (local-out OUT)
        ]
     (out 0 (pan2 OUT))))


;(defn arp5 [i]
;  (let [start-note (:note state)
;        arp-speed (:speed state)
;        times [1 1 1 1 1 1 1 1 1]
;        arp-time (nth times (mod i (count times)))]
;    (println "arp5: speed " arp-speed " i " i)
;    (if (== i 0)
;        (ctl synth0 :note start-note))
;    (apply-at (+ (now) (* arp-time arp-speed))
;      (fn [] 
;        (let [notes [0 7 12 19 24 19 12 7]
;              note  (+ (:note state) (nth notes (mod i (count notes))))]
;          (println "arp5 " note)
;          (ctl synth0 :note note)
;          (println "arp5-gate-state:" (:gate state))
;          (if (and (not= 0 (:gate state)) (== (:note state) start-note))
;              (do (println "recursing")
;                  (arp5 (inc i)))
;              (println "arp stop")))))))
;

