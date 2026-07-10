(ns kami.animation "Portable immutable keyframe and timeline evaluator.")

(defn keyframe [time value] {:keyframe/time time :keyframe/value value})
(defn track [target keyframes] {:track/target target :track/keyframes (vec (sort-by :keyframe/time keyframes))})
(defn- lerp [a b t] (+ a (* (- b a) t)))
(defn sample
  "Sample a scalar track at time. Values clamp outside its keyframe range."
  [{:track/keys [keyframes]} time]
  (when-not (seq keyframes) (throw (ex-info "track needs a keyframe" {})))
  (let [first-k (first keyframes) last-k (last keyframes)]
    (cond (<= time (:keyframe/time first-k)) (:keyframe/value first-k)
          (>= time (:keyframe/time last-k)) (:keyframe/value last-k)
          :else (let [[a b] (first (filter (fn [[a b]] (<= (:keyframe/time a) time (:keyframe/time b))) (partition 2 1 keyframes)))
                      p (/ (- time (:keyframe/time a)) (- (:keyframe/time b) (:keyframe/time a)))]
                  (lerp (:keyframe/value a) (:keyframe/value b) p)))))

(defn timeline [duration tracks] {:timeline/duration duration :timeline/tracks (vec tracks)})
(defn evaluate [timeline time] (into {} (map (fn [t] [(:track/target t) (sample t time)]) (:timeline/tracks timeline))))
