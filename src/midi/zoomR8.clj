(ns midi.zoomR8
  (:use [overtone.midi]
        [overtone.libs.event :only [event]]))

(defrecord ZoomR8 [name in interfaces state])

(def event-handle "/overtone/midi/zoomR8")

(def config-zoomR8
  {:name "Zoom R8"
   :interfaces {:input-controls {:name "Input Controls"
                                 :type :midi-in
                                 :midi-handle "ZOOM R8"
                                 :control-defaults {:chan 0 :cmd 176 :type :button}
                                 :controls {:audio-punch-io {:note 54 :cmd 144}
                                            :a-b-repeat{:note 55 :cmd 144}
                                            :beginning {:note 56 :cmd 144}
                                            :end {:note 57 :cmd 144}
                                            :mark-clear {:note 58 :cmd 144}
                                            :rewind {:note 91 :cmd 144}
                                            :fast-forward {:note 92 :cmd 144}
                                            :stop {:note 93 :cmd 144}
                                            :play {:note 94 :cmd 144}
                                            :record {:note 95 :cmd 144}

                                            ;; sliders
                                            :c1 {:chan 0 :note 0 :cmd 224 :type :slider}
                                            :c2 {:chan 1 :note 0 :cmd 224 :type :slider}
                                            :c3 {:chan 2 :note 0 :cmd 224 :type :slider}
                                            :c4 {:chan 3 :note 0 :cmd 224 :type :slider}
                                            :c5 {:chan 4 :note 0 :cmd 224 :type :slider}
                                            :c6 {:chan 5 :note 0 :cmd 224 :type :slider}
                                            :c7 {:chan 6 :note 0 :cmd 224 :type :slider}
                                            :c8 {:chan 7 :note 0 :cmd 224 :type :slider}
                                            :c9 {:chan 8 :note 0 :cmd 224 :type :slider}

                                            ;; buttons
                                            :b1 {:note 8  :cmd 144}
                                            :b2 {:note 9  :cmd 144}
                                            :b3 {:note 10 :cmd 144}
                                            :b4 {:note 11 :cmd 144}
                                            :b5 {:note 12 :cmd 144}
                                            :b6 {:note 13 :cmd 144}
                                            :b7 {:note 14 :cmd 144}
                                            :b8 {:note 15 :cmd 144}
                                            
                                            :up    {:note 96 :cmd 144}
                                            :down  {:note 97 :cmd 144}
                                            :left  {:note 98 :cmd 144}
                                            :right {:note 99 :cmd 144}
                                            :wheel {:note 60 :cmd 176}}}}})
                                            


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

(defn- note-controls-map
  [config]
  (let [controls (-> config :interfaces :input-controls :controls)]
    (into {}
          (map (fn [[k v]] [(:note v) k])
               controls))))

(defn zoomR8-midi->control
    ([msg ts] (zoomR8-midi->control msg ts (merge-control-defaults config-zoomR8)))
    ([msg ts config]
     (let [controls (-> config :interfaces :input-controls :controls)
           note (:note msg)
           vel  (:vel msg)
           cmd  (:cmd msg)
           chan (:chan msg)]
           (if-let [matched-control (some (fn [e] (if (and (= chan (:chan (second e)))
                                                           (= note (:note (second e)))
                                                           (= cmd  (:cmd  (second e))))
                                                      (first e)))
                                          controls)]
                   {:ts ts :val vel :id matched-control}))))

(defn- mk-midi-in-handler
  [config state f]
  (let [controls (-> config :interfaces :input-controls :controls)]
    (fn [msg ts]
      (let [note   (:note msg)
            vel    (:vel msg)
            cmd    (:cmd msg)
            chan   (:chan msg)
            matched-control (some (fn [e] (if (and (= chan (:chan (second e)))
                                                   (= note (:note (second e)))
                                                   (= cmd  (:cmd  (second e))))
                                          (first e)))
                                   controls)
           ]
        (println "Zoom R8 " msg)
        (if matched-control
            (f {:ts ts :val vel :id matched-control}))))))

(defn zoomR8-midi-handler
  "Create a midi handler for a ZOOM R8 midi device. If your device
   doesn't have the midi-handle \"ZOOM R8\" for the input controls
   (perhaps if you are connecting more than one simulataneously)
   you may also specify these identifiers directly."
   ([] (zoomR8-midi-handler event))
   ([f] (zoomR8-midi-handler (-> config-zoomR8 :interfaces :input-controls :midi-handle) f))
   ([midi-in-str f]
     (let [in         (midi-in midi-in-str)
           config     (merge-control-defaults config-zoomR8)
           interfaces (-> config :interfaces)
           state      (into {} (map (fn [[k v]] [k (atom nil)]) (-> config :interfaces :input-controls :controls)))]
           (mk-midi-in-handler config state f))))

(defn connect-zoomR8
  "Connect to a connected Zoom R8 midi device. If your device
  doesn't have the midi-handle \"ZOOM R8\" for the input
  controls (perhaps if you are connecting more than one
  simulataneously) you may also specify these identifiers directly."
  ([] (connect-zoomR8 (-> config-zoomR8 :interfaces :input-controls :midi-handle)))
  ([midi-in-str]
     (let [in         (midi-in midi-in-str)
           config     (merge-control-defaults config-zoomR8)
           interfaces (-> config :interfaces)
           state      (into {} (map (fn [[k v]] [k (atom nil)]) (-> config :interfaces :input-controls :controls)))]
       (midi-handle-events in (mk-midi-in-handler config state event-handle))
       (ZoomR8. (:name config) in interfaces state))))

