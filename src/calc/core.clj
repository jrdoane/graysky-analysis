(ns calc.core
  (:require
    [clojure-csv.core :as csv])
  (:import (java.io File)))

(defn ->int
  [i]
  (when i
    (Integer/parseInt i)))

(defn read-csv
  [file-path]
  (when (.isFile (File. file-path))
    (set
      (map
        (fn [i]
          (when (:time i)
            (-> i
                (assoc :time (->int (:time i)))
                (assoc :record (->int (:record i)))
                (assoc :core0 (->int (:core0 i)))
                (assoc :core1 (->int (:core1 i)))
                (assoc :core2 (->int (:core2 i)))
                (assoc :core3 (->int (:core3 i)))
                (assoc :pwm (->int (:pwm i)))
                (assoc :fan (->int (:fan i)))
                (assoc :ambient (->int (:ambient i))))))
        (let [parsed (csv/parse-csv (slurp file-path))
              columns (map keyword (first parsed))]
          (map (partial zipmap columns)
               (rest parsed)))))))

(defn fahrenheit->celsius [i] (/ (- i 32.0) 1.8))

(defn csv->by-time
  [parsed-csv]
  (let
  [time-map
   (into
     {}
     (map
       (fn [i] [(:time (first i)) (second i)])
       (clojure.set/index parsed-csv [:time])))
   ordered-times (sort < (keys time-map))
   max-temp-ordered-times
   (map
     (fn [i]
       (let [items (get time-map i)]
         [(apply
            max
            (concat
              (map :core0 items)
              (map :core1 items)
              (map :core2 items)
              (map :core3 items)))
          (apply
            max
            (map fahrenheit->celsius
                 (map :ambient items)))]))
     ordered-times)]
  (loop
    [delta-seq []
     last-values (first max-temp-ordered-times)
     remaining-times (rest max-temp-ordered-times)]
    (if (empty? remaining-times)
      delta-seq
      (let [current (first remaining-times)
            delta-cpu (- (first current) 0.0 (first last-values))
            delta-ambient (- (second current) 0.0 (second last-values))
            delta-delta (- delta-cpu delta-ambient)]
      (recur
        (conj delta-seq [delta-cpu delta-ambient delta-delta])
        current
        (rest remaining-times)))))))

(comment

  (def n (read-csv "/storage/as5.csv"))
  (def mprime-set
    (filter
      (fn [i] (= (:stresser i) "mprime-440k"))
      n))
  (def burn-set
    (filter
      (fn [i] (= (:stresser i) "burnP6"))
      n))
  (def mprime-numbers (csv->by-time mprime-set))
  (def burn-numbers (csv->by-time burn-set))
  
  (spit "/storage/burn.csv"
        (csv/write-csv
          (map (partial map str) burn-numbers)))
  (spit "/storage/mprime.csv"
        (csv/write-csv
          (map (partial map str) mprime-numbers)))

  )
