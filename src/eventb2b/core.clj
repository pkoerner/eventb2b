(ns eventb2b.core
  (:use [lisb.translation.lisb2ir]
        [lisb.translation.irtools])
  (:require [clojure.set :as sett]
            [clojure.java.io :refer [file]]
            [com.rpl.specter :as s]
            [lisb.translation.util :refer [ir->ast ast->b]] 
            [lisb.translation.eventb.util :refer [rodin->lisb eventb ir->prob-model prob-model->rodin]])
  (:import java.io.File)
  (:gen-class))

(defn merge-irs [irs]
  (bmachine :merged
     (apply bsets
       (map #(apply benumerated-set (:set %) (map first (:partitions %)))
               (filter #(= (:tag %) :partition)
                       (distinct (mapcat #(:values (s/select-one (CLAUSE :properties) %)) irs)))))
     (apply bconstants
            (seq (sett/difference (set (distinct (mapcat #(:values (s/select-one (CLAUSE :constants) %)) irs)))
                                  (set (apply concat (mapcat :partitions (filter #(= (:tag %) :partition)
                                                                                 (distinct (mapcat #(:values (s/select-one (CLAUSE :properties) %)) irs))))))
                                  )))
     (apply bproperties
       (remove #(= (:tag %) :partition) (distinct (mapcat #(:values (s/select-one (CLAUSE :properties) %)) irs))))
     (apply bvariables
       (distinct (mapcat #(:values (s/select-one (CLAUSE :variables) %)) irs)))
     (apply binvariants
       (distinct (mapcat #(:values (s/select-one (CLAUSE :invariants) %)) irs)))
     (binit
       (apply bparallel-sub (distinct (mapcat #(:values (s/select-one (CLAUSE :init) %)) irs))) )
     (apply boperations
            (let [ops (group-by :name (mapcat #(:values (s/select-one (CLAUSE :events) %)) irs))]
              (for [[opname events] ops
                    :let [evt (last events)]]
                (bop opname
                     (list* (:values (first (filter #(= :args (:tag %)) (:clauses evt)))))
                     (bselect (apply band (:values (first (filter #(= :guards (:tag %)) (:clauses evt)))))
                              (apply bparallel-sub (:values (first (filter #(= :actions (:tag %)) (:clauses evt))))))))
              ))))


(defn make-flät [irs]
  (let [ir (dissoc (assoc (last irs) :name "Flæt" :tag :machine) :abstract-machine-name)
        ir-half (update ir :machine-clauses (partial remove #(= (:tag %) :events)))
        invs (apply binvariants (mapcat :values (filter #(= (:tag %) :invariants) (mapcat :machine-clauses (filter #(not= :context (:tag %)) irs)))))
        events (first (filter #(= (:tag %) :events) (:machine-clauses ir)))
        events' (update events :values
                        (fn [values] (map (fn [event]
                                            (update event :clauses
                                                    (partial remove (fn [x] (= (:tag x) :event-reference)) )))
                                          values)))]
    (update ir-half :machine-clauses concat [invs events'])))

(defn -main
  [& args]
  (when (not= (count args) 2)
    (println "Usage: java -jar eventb2b-0.1.0-SNAPSHOT-standalone.jar input.bum output.mch")
    (System/exit 1))
  (let [[input output] args
        xx (rodin->lisb input)
        irs (map (fn [x] (eval `(eventb ~x))) xx)
        b (ast->b (ir->ast (merge-irs irs)) :indent "  ")
        f (file output)]
    (.mkdirs (.getParentFile f))
    (spit f b)
    (Thread/sleep 1000) ;; TODO: kill ProB
    (System/exit 0)))

(defn flachmann [input]
  (let [xx (rodin->lisb input)
        irs (map (fn [x] (eval `(eventb ~x))) xx)]
    (prob-model->rodin (ir->prob-model (make-flät irs))
                       "Drone_Exercise5" "/home/philipp/Downloads/")))

(comment 
  "works on my machine"
  (flachmann "/home/philipp/Downloads/Drone_Exercise5/M7_DroneCriticalSafetyDistance_InstFull.bum")
  (-main "/home/philipp/Downloads/Drone_Exercise5/M7_DroneCriticalSafetyDistance_InstFull.bum" "./foo.mch"))



