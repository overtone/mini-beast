(ns midi.oxygen61
  (:use [overtone.midi]
        [overtone.libs.event :only [event]]))

(defrecord Oxygen61 [name in interfaces state])

(def event-handle "/overtone/midi/oxygen61")

(def config-oxygen61
  {:name "oxygen61"
   :interfaces {:input-controls {:name "Input Controls"
                                 :type :midi-in
                                 :midi-handle "Oxygen 61"
                                 :control-defaults {:chan 0 :cmd 176 :type :button}
                                 :controls {:cycle {:note 113}
                                            :rewind {:note 114}
                                            :fast-forward {:note 115}
                                            :stop {:note 116}
                                            :play {:note 117}
                                            :record {:note 118}

                                            ;; sliders
                                            :c1 {:note 74 :type :slider}
                                            :c2 {:note 71 :type :slider}
                                            :c3 {:note 91 :type :slider}
                                            :c4 {:note 93 :type :slider}
                                            :c5 {:note 73 :type :slider}
                                            :c6 {:note 72 :type :slider}
                                            :c7 {:note 5  :type :slider}
                                            :c8 {:note 84 :type :slider}
                                            :c9 {:note 7  :type :slider}

                                            ;; pots                                            
                                            :c10 {:note 75 :type :pot}
                                            :c11 {:note 76 :type :pot}
                                            :c12 {:note 92 :type :pot}
                                            :c13 {:note 95 :type :pot}
                                            :c14 {:note 10 :type :pot}
                                            :c15 {:note 77 :type :pot}
                                            :c16 {:note 78 :type :pot}
                                            :c17 {:note 79 :type :pot}

                                            ;; buttons
                                            :c18 {:note 0 :cmd 192}
                                            :c19 {:note 1 :cmd 192}
                                            :c20 {:note 2 :cmd 192}
                                            :c21 {:note 3 :cmd 192}
                                            :c22 {:note 4 :cmd 192}
                                            :c23 {:note 5 :cmd 192}
                                            :c24 {:note 6 :cmd 192}
                                            :c25 {:note 7 :cmd 192}
                                            :c26 {:note 8 :cmd 192}}}}})
                                            


(defn- merge-control-defaults
  "Returns config map where control info maps are merged
  with :control-defaults map."
  [config]
  (assoc config
    :interfaces
    (into {}
          (map (fn [[i-name i-info]]
                 [i-name (assoc (dissoc i-info :control-defaults)
                           :controls (into {}
                                           (map (fn [[c-name c-info]]
                                                  [c-name (merge (:control-defaults i-info)
                                                                 c-info)])
                                                (:controls i-info))))])
               (:interfaces config)))))

(defn note-controls-map
  [config]
  (let [controls (-> config :interfaces :input-controls :controls)]
    (into {}
          (map (fn [[k v]] [(:note v) k])
               controls))))

(defn oxygen61-midi->control
   ([msg ts] (oxygen61-midi->control msg ts (merge-control-defaults config-oxygen61)))
   ([msg ts config]
    (let [controls (-> config :interfaces :input-controls :controls)
          note   (:note msg)
          vel    (:vel msg)
          cmd    (:cmd msg)
          matched-control (some (fn [e] (if (and (= note (:note (second e)))
                                                 (= cmd  (:cmd  (second e))))
                                            (first e )))
                                        controls)
          at-key (if (some #{(:cmd msg)} [128 144]) note matched-control)]
          {:ts ts :val vel :id at-key})))

(defn mk-midi-in-handler
  [config state f]
  (let [controls (-> config :interfaces :input-controls :controls)]
    (fn [msg ts]
      (let [note   (:note msg)
            vel    (:vel msg)
            cmd    (:cmd msg)
            matched-control (some (fn [e] (if (and (= note (:note (second e)))
                                                    (= cmd  (:cmd  (second e))))
                                                 (first e ))) controls)
            at-key (if (some #{(:cmd msg)} [128 144]) note matched-control)
            ;atm    (get state at-key)
           ]
        ;(reset! atm vel)
        (f {:ts ts :val vel :id at-key})))))

(defn oxygen61-midi-handler
  "Create a midi handler for an Oxygen 61 midi device. If your device
  doesn't have the midi-handle \"Oxygen 61\" for the input
  controls (perhaps if you are connecting more than one
  simulataneously) you may also specify these identifiers directly."
  ([] (oxygen61-midi-handler event))
  ([f] (oxygen61-midi-handler (-> config-oxygen61 :interfaces :input-controls :midi-handle) f))
  ([midi-in-str f]
     (let [config     (merge-control-defaults config-oxygen61)
           interfaces (-> config :interfaces)
           state      (into {} (map (fn [[k v]] [k (atom nil)]) (-> config :interfaces :input-controls :controls)))]
       (mk-midi-in-handler config state f))))


(defn connect-oxygen61
  "Connect to a connected Oxygen 61 midi device. If your device
  doesn't have the midi-handle \"Oxygen 61\" for the input
  controls (perhaps if you are connecting more than one
  simulataneously) you may also specify these identifiers directly."
  ([] (connect-oxygen61 (-> config-oxygen61 :interfaces :input-controls :midi-handle)))
  ([midi-in-str]
     (let [in         (midi-in midi-in-str)
           config     (merge-control-defaults config-oxygen61)
           interfaces (-> config :interfaces)
           state      (into {} (map (fn [[k v]] [k (atom nil)]) (-> config :interfaces :input-controls :controls)))]
       (midi-handle-events in (oxygen61-midi-handler midi-in-str))
       (Oxygen61. (:name config) in interfaces state))))

