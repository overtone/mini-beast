(ns minibeast.mbsynth
  (:use [overtone.live]))

(defsynth darp
  [arp-trig-bus      {:default 1      :doc "bus to output arp trigger"}
   arp-note-bus      {:default 1      :doc "bus to output arp notes"}
   arp-rate          {:default 2.0    :doc "Rate of arpeggiation in Hz"}
   arp-swing-phase   {:default 0      :doc "phase offset of swung (swinged?) notes. Degrees."}
   arp-range         {:default 2      :doc "Octave range of arpeggiation"}
   arp-mode          {:default 0      :doc "0 = off, 1 = up, 2 = down, 3 = up/down, 4 = random"}
   arp-step          {:default 0      :doc "0 = 1/4, 1 = 1/8, 2 = 1/16, 3 = 1/4T, 4 = 1/8T 5 = 1/16T"}]
  (let [arp-on?         (min arp-mode 1)
        rate            (* arp-rate (select:kr arp-step [(dc:kr 1) (dc:kr 2) (dc:kr 4) (dc:kr 3) (dc:kr 6) (dc:kr 12)]))
        arp-trig        (+ (impulse (/ rate 2))
                           (impulse (/ rate 2) (+ (* arp-swing-phase (/ 1 360.0))  0.5)))
        arp-tap         (tap :arp 10 (lag-ud arp-trig 0 0.9))
        arp-scale-up    (dseries 0                (dseq [ 7  5] INF) (+ (* arp-range 2) 1))
        arp-scale-down  (dseries (* 12 arp-range) (dseq [-5 -7] INF) (+ (* arp-range 2) 1))
        arp-notes       (dswitch1 [(donce  0)
                                   (dseq  arp-scale-up INF)
                                   (dseq  arp-scale-down INF)
                                   ;; notes at the top and bottom repeat. May be due to
                                   ;; http://goo.gl/iGVJb
                                   (dswitch [(dser arp-scale-up   (* 2 arp-range))
                                             (dser arp-scale-down (* 2 arp-range))] (dseq [0 1] INF))
                                   (dshuf arp-scale-up INF)] arp-mode)
        arp-out         (demand arp-trig 0 arp-notes INF)]
    (out:kr arp-trig-bus (* arp-on? arp-trig))
    (out:kr arp-note-bus arp-out)))
        
(defsynth LFO
  [lfo-bus           {:default 16     :doc "bus to output LFO"}
   arp-trig-bus      {:default 1      :doc "bus to input arp trigger"}
   lfo-rate          {:default 4.0    :doc "frequency of the LFO"}
   lfo-sin           {:default 1.0    :doc "LFO sin amount"}
   lfo-saw           {:default 0.0    :doc "LFO saw amount"}
   lfo-square        {:default 0.0    :doc "LFO square amount"}
   lfo-tri           {:default 0.0    :doc "LFO triange amount"}
   lfo-rand          {:default 0.0    :doc "LFO random amount"}
   lfo-slew-rand     {:default 0.0    :doc "LFO smoothed random amount"}
   lfo-arp-sync      {:default 0      :doc "0 = no sync, 1 = sync"}]
  (let [
        rand-lfo        (t-rand -1.0 1.0 (sin-osc lfo-rate)) 
        arp-trig        (in:kr arp-trig-bus 1)
        phase-reset     (* lfo-arp-sync (+ 1 (* -1 arp-trig)))
        phase           (wrap (sweep phase-reset (* 2 Math/PI lfo-rate)) (- Math/PI) Math/PI)
        LFO             (slew (+ (* lfo-sin (sin-osc 0 phase))
                                 (* lfo-saw (lf-saw 0 phase))
                                 (* lfo-square (square 0 phase))
                                 (* lfo-tri (lf-tri 0 phase))
                                 (* lfo-rand rand-lfo)
                                 (* lfo-slew-rand (slew rand-lfo))) 100 100)
        lfo-tap         (tap :lfo 10
                             (lag-ud LFO 0 0.9))]
    (out lfo-bus LFO)))


(defsynth voice
  [voice-bus         {:default 0      :doc "bus to output voice"}
   lfo-bus           {:default 16     :doc "bus of lfo"}
   arp-trig-bus      {:default 17     :doc "bus of arp trigger"}
   arp-note-bus      {:default 17     :doc "bus of arp notes"}
   note              {:default 60     :doc "midi note value"}
   bend              {:default 0.0    :doc "-1 to 1"}
   bend-range        {:default 12.0   :doc "number of semitones of a maximum bend"}
   velocity          {:default 1.0    :doc "gain for the current note"}
   octave-transpose  {:default 0.0    :doc "number of octaves to transpose notes. -2 to 2"}
   portamento        {:default 0.0    :doc "rate to change to new note"}
   gate              {:default 0.0    :doc "ADSR trigger"}
   cutoff            {:default 1000.0 :doc "cutoff frequency of the VCF"}
   resonance         {:default 1.0    :doc "resonance of the VCF"}
   env-speed         {:default 1.0    :doc "envelope speed modifier. Modifies ADR speeds by multiplying."}
   filter-type       {:default 0      :doc "0 = low pass, 1 = bandpass, 2 = highpass, 3 = notch"}
   filter-attack     {:default 0.0    :doc "Filter envelope attack"}
   filter-decay      {:default 0.0    :doc "Filter envelope decay"}
   filter-sustain    {:default 0.0    :doc "Filter envelope sustain"}
   filter-release    {:default 0.0    :doc "Filter envelope release"}
   amp-attack        {:default 0.1    :doc "Amp envelope attack"}
   amp-decay         {:default 0.2    :doc "Amp envelope decay"}
   amp-sustain       {:default 0.2    :doc "Amp envelope sustain"}
   amp-release       {:default 0.2    :doc "Amp envelope release"}
   osc-saw           {:default 1.0    :doc "osc saw amount"}
   osc-square        {:default 0.0    :doc "osc square amount"}
   osc-tri           {:default 0.0    :doc "osc triangle amount"}
   osc-noise         {:default 0.0    :doc "osc noise amount"}
   osc-audio-in      {:default 0.0    :doc "audio-in amount"}
   sub-osc-sin       {:default 0.0    :doc "sin sub-oscillator"}
   sub-osc-square    {:default 0.0    :doc "square sub-oscillator"}
   sub-osc-coeff     {:default 0.5    :doc "0.5 = -1 octave 0.25 = -2 octave"}
   saw-detune        {:default 1.0    :doc "phase offset of parallel saw oscs"}
   saw-detune-amp    {:default 1.0    :doc "amount of detuned saw osc in the mix"}
   osc-square-pw     {:default 0.5    :doc "square osc pulse width"}
   osc-square-pw-env {:default 0.0    :doc "pw modulation by amp envelope"}
   tri-fold-thresh   {:default 1.0    :doc "fold threshold for triangle osc"}
   tri-fold-env      {:default 0.0    :doc "fold threshold env modulation for triangle osc"}
   cutoff-env        {:default 0.0    :doc "cutoff envelope modulation"}
   cutoff-tracking   {:default 1.0    :doc "keyboard tracking amount for filter cutoff"}
   lfo2pitch         {:default 0.0    :doc "LFO pitch modulation"}
   lfo2filter        {:default 0.0    :doc "LFO filter modulation"}
   lfo2amp           {:default 0.0    :doc "LFO amp modulation"}
   lfo2pwm           {:default 0.0    :doc "LFO PWM modulation"}
   vibrato-rate      {:default 0.0    :doc "vibrato rate Hz"}
   vibrato-amp       {:default 0.0    :doc "amount of vibrato modulation 0.0-1.0"}
   vibrato-trill     {:default 0.0    :doc "trill amount. 0 is no trill. 1 is trill up one half-step. -1 is trill down one half-step"}
   feedback-amp      {:default 0.0    :doc "feedback amount"}
   ]
  (let [
        arp-trig        (in:kr arp-trig-bus 1)
        arp-notes       (in:kr arp-note-bus 1)
        gate-with-arp   (- gate arp-trig)
        AMP-ADSR        (env-gen (adsr (* amp-attack env-speed)
                                       (* amp-decay env-speed)
                                       amp-sustain
                                       (* amp-release env-speed)) gate-with-arp)
        amp-adsr-tap    (tap :amp-adsr 10
                             AMP-ADSR)
        FILTER-ADSR     (env-gen (adsr (* filter-attack env-speed)
                                       (* filter-decay env-speed)
                                       filter-sustain
                                       (* filter-release env-speed)) gate-with-arp)
        filter-adsr-tap (tap :filter-adsr 10
                             FILTER-ADSR)
        input-note-freq (midicps (+ note
                                    (* octave-transpose 12)
                                    (* bend bend-range)
                                    (* (lf-pulse vibrato-rate) vibrato-trill)
                                    arp-notes))
        glide-rate      (/ input-note-freq (max portamento 0.001))
        LFO             (in lfo-bus 1)
        note-freq       (slew (+ input-note-freq
                                 (* lfo2pitch LFO))
                              glide-rate glide-rate)
        sub-note-freq   (* note-freq sub-osc-coeff)
        TRI-FOLD-THRESH (max 0.01 (+ (* FILTER-ADSR tri-fold-env) tri-fold-thresh))
        VCO             (+ (* osc-saw (+ (lf-saw note-freq) 
                                         (* saw-detune-amp
                                            (lf-saw (+ note-freq saw-detune))
                                            (lf-saw (- note-freq saw-detune))
                                            (lf-saw (+ note-freq (* 2 saw-detune)))
                                            (lf-saw (- note-freq (* 2 saw-detune))))))
                           (* osc-square (pulse note-freq (+ (* FILTER-ADSR osc-square-pw-env) osc-square-pw (* LFO lfo2pwm))))
                           (* osc-tri (/ 1.0 TRI-FOLD-THRESH) (fold2 (lf-tri note-freq) TRI-FOLD-THRESH))
                           (* osc-noise (white-noise))
                           (* sub-osc-sin (sin-osc sub-note-freq))
                           (* sub-osc-square (square sub-note-freq (* LFO lfo2pwm)))
                           (* osc-audio-in (sound-in)))

        VCO+fback       (+ VCO (* feedback-amp (local-in))) 
        vcf-freq        (max 20 (+ cutoff
                                  (* lfo2filter LFO)
                                  (* cutoff-tracking note-freq)
                                  (* cutoff-env FILTER-ADSR)))
        filter-bank    [(rlpf:ar VCO+fback vcf-freq (* -1 (- resonance 4)))
                        (bpf:ar  VCO+fback vcf-freq (* -1 (- resonance 4)))
                        (rhpf:ar VCO+fback vcf-freq (* -1 (- resonance 4)))
                        (brf:ar  VCO+fback vcf-freq (* -1 (- resonance 4)))]
        VCF             (select filter-type filter-bank)

        VIBRATO-LFO     (+ 1 (* vibrato-amp (sin-osc:kr vibrato-rate)))
        VCA             (* (+ 1 (*  lfo2amp LFO))
                           AMP-ADSR 
                           VIBRATO-LFO)
        OUT             (softclip (* velocity VCA VCF))
        _               (local-out OUT)
        ]
    (out voice-bus OUT)))


(defsynth mbsynth
  [
   voice-bus         {:default 18     :doc "bus of voices"}
   volume            {:default 1.0    :doc "gain of the output"}
   reverb-mix        {:default 0.5    :doc "wet-dry mix"}
   reverb-size       {:default 0.3    :doc "reverb room size"}
   reverb-damp       {:default 0.8    :doc "reverb dampening"}
   delay-mix         {:default 0.0    :doc "wet-dry mix"}
   delay-feedback    {:default 0.2    :doc "amount of delay to feedback into delay loop. 0..1"}
   delay-time        {:default 0.8    :doc "delay time in seconds"}
   ]
  (let [
        voices          (in voice-bus 1)
        delay-in        (local-in)
        mix             (* volume voices)
        delay-sig       (delay-n (+ mix
                                    (* delay-feedback delay-in))
                                 1.0 delay-time)
        delay-mix       (softclip (+ (* delay-mix delay-sig) mix))
        OUT             (free-verb delay-mix reverb-mix reverb-size reverb-damp)
        _               (local-out delay-sig)
        ]
    (out 0 (pan2 OUT))))

