(ns event.arp
  (:use ;[overtone.at-at]
        [overtone.music.time]))

(defrecord Arp [start-note last-note speed gate])

;(def arp (atom (Arp. 0 0 200 0)))

(defn arp-on? [arp] (= 1 (:gate arp)))

(defn arp-off? [arp] (= 0 (:gate arp)))

(defn arp-off [arp note]
    (let [arp-start-note (:start-note @arp)]
         (if (= note arp-start-note)
               (do (println "arp on -> turning off")
                   (swap! arp #(merge % {:gate 0}))))))

(defn arp-on [arp start-note noteon-fn noteoff-fn]
    ;; only turn arp on if arp is off
    (if (arp-off? @arp)
        (do
            (noteoff-fn (:last-note @arp))
            ;; set the arp's start note, last note and turn the gate on
            (swap! arp #(merge % {:start-note start-note
                                  :last-note start-note
                                  :gate 1}))
            ;; In the future, run this fn
            ((fn arp-next [i]
                (apply-at (+ (now) (:speed @arp))
                    (fn []
                    (let [last-note (:last-note @arp)]
                         ;; turn the old note off
                         (noteoff-fn last-note)
                         ;; only play new note if arp is still on
                         (if (arp-on? @arp)
                             ;; pick the new note to play
                             (let [notes [0 7 12 19 24 19 12 7]
                                   note  (+ (:start-note @arp) (nth notes (mod i (count notes))))]
                                   ;; turn the new note on
                                   (noteon-fn note)
                                   ;; set the last note to the value of the new note
                                   (swap! arp #(merge % {:last-note note}))
                                   ;; recurse!
                                   (arp-next (inc i)))))))) 1))))


;;previously in minibeast.core:

;; Arp on-off
;;:c20 (let [new-state (alter-state
;;              #(assoc % :arp-on (not (:arp-on %))))]
;;          (if (:arp-on @state)
;;              ;; Add arp handler
;;              (on-event midi.oxygen61/event-handle
;;                  (fn [event]
;;                      (if-let [id (:id event)]
;;                          (if (not (keyword? id))
;;                              (let [val (:val event)]
;;                                    ;; key event
;;                                    (if (= val 0)
;;                                        ;; Key up
;;                                        (arp-off arp id)
;;                                        ;; Key down
;;                                        (arp-on arp id keydown keyup)))))) ::oxygen61-arp-handler)
;;              ;; Remove arp handler
;;              (remove-handler midi.oxygen61/event-handle ::oxygen61-arp-handler)))
