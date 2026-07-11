(ns kami.animation "Portable immutable keyframe and timeline evaluator.")

(defn keyframe
  ([time value] (keyframe time value :linear))
  ([time value interpolation]
   {:keyframe/id (random-uuid) :keyframe/time time :keyframe/value value
    :keyframe/interpolation interpolation}))
(defn track [target keyframes]
  {:track/target target :track/keyframes (vec (sort-by :keyframe/time keyframes))})
(defn- lerp [a b t] (+ a (* (- b a) t)))
(defn- smoothstep [t] (* t t (- 3 (* 2 t))))
(defn- interpolate [a b p kind]
  (case kind :step a :smooth (lerp a b (smoothstep p)) (lerp a b p)))
(defn sample
  "Sample a scalar track at time. Values clamp outside its keyframe range."
  [{:track/keys [keyframes]} time]
  (when-not (seq keyframes) (throw (ex-info "track needs a keyframe" {})))
  (let [first-k (first keyframes) last-k (last keyframes)]
    (cond (<= time (:keyframe/time first-k)) (:keyframe/value first-k)
          (>= time (:keyframe/time last-k)) (:keyframe/value last-k)
          :else (let [[a b] (first (filter (fn [[a b]] (<= (:keyframe/time a) time (:keyframe/time b))) (partition 2 1 keyframes)))
                      p (/ (- time (:keyframe/time a)) (- (:keyframe/time b) (:keyframe/time a)))]
                  (interpolate (:keyframe/value a) (:keyframe/value b) p
                               (:keyframe/interpolation a))))))

(defn timeline [duration tracks] {:timeline/duration duration :timeline/tracks (vec tracks)})
(defn evaluate [timeline time] (into {} (map (fn [t] [(:track/target t) (sample t time)]) (:timeline/tracks timeline))))

(defn add-keyframe [timeline target frame]
  (update timeline :timeline/tracks
          (fn [tracks]
            (if-let [i (first (keep-indexed #(when (= target (:track/target %2)) %1) tracks))]
              (update tracks i #(track target (conj (:track/keyframes %) frame)))
              (conj tracks (track target [frame]))))))

(defn update-keyframe [timeline target frame-id f & args]
  (update timeline :timeline/tracks
          (fn [tracks]
            (mapv (fn [t]
                    (if (= target (:track/target t))
                      (track target (mapv #(if (= frame-id (:keyframe/id %))
                                             (apply f % args) %) (:track/keyframes t)))
                      t)) tracks))))

(defn move-keyframe [timeline target frame-id time]
  (update-keyframe timeline target frame-id assoc :keyframe/time time))

(defn delete-keyframe [timeline target frame-id]
  (update timeline :timeline/tracks
          (fn [tracks]
            (->> tracks
                 (mapv (fn [t]
                         (if (= target (:track/target t))
                           (track target (remove #(= frame-id (:keyframe/id %))
                                                 (:track/keyframes t))) t)))
                 (remove #(empty? (:track/keyframes %))) vec))))
