(ns kami.animation "Portable immutable keyframe and timeline evaluator.")

(defn keyframe
  ([time value] (keyframe time value :linear))
  ([time value interpolation]
   (keyframe time value interpolation {}))
  ([time value interpolation {:keys [tangent-in tangent-out broken?]
                              :or {tangent-in 0.0 tangent-out 0.0 broken? false}}]
   (when (neg? time) (throw (ex-info "keyframe time cannot be negative" {:time time})))
   {:keyframe/id (random-uuid) :keyframe/time time :keyframe/value value
    :keyframe/interpolation interpolation :keyframe/tangent-in tangent-in
    :keyframe/tangent-out tangent-out :keyframe/broken? broken?}))
(defn track [target keyframes]
  {:track/target target :track/keyframes (vec (sort-by :keyframe/time keyframes))})
(defn- lerp [a b t] (+ a (* (- b a) t)))
(defn- smoothstep [t] (* t t (- 3 (* 2 t))))
(defn- hermite [a b p tangent-out tangent-in duration]
  (let [p2 (* p p) p3 (* p2 p)
        h00 (+ (* 2 p3) (* -3 p2) 1) h10 (+ p3 (* -2 p2) p)
        h01 (+ (* -2 p3) (* 3 p2)) h11 (- p3 p2)]
    (+ (* h00 a) (* h10 duration tangent-out) (* h01 b) (* h11 duration tangent-in))))
(defn- interpolate [a b p kind tangent-out tangent-in duration]
  (case kind :step a :smooth (lerp a b (smoothstep p))
        :bezier (hermite a b p tangent-out tangent-in duration)
        :hermite (hermite a b p tangent-out tangent-in duration)
        (lerp a b p)))
(defn sample
  "Sample a scalar track at time. Values clamp outside its keyframe range."
  [{:track/keys [keyframes]} time]
  (when-not (seq keyframes) (throw (ex-info "track needs a keyframe" {})))
  (let [first-k (first keyframes) last-k (last keyframes)]
    (cond (<= time (:keyframe/time first-k)) (:keyframe/value first-k)
          (>= time (:keyframe/time last-k)) (:keyframe/value last-k)
          :else (let [[a b] (first (filter (fn [[a b]] (<= (:keyframe/time a) time (:keyframe/time b))) (partition 2 1 keyframes)))
                      duration (- (:keyframe/time b) (:keyframe/time a))
                      p (/ (- time (:keyframe/time a)) duration)]
                  (interpolate (:keyframe/value a) (:keyframe/value b) p
                               (:keyframe/interpolation a)
                               (:keyframe/tangent-out a 0.0) (:keyframe/tangent-in b 0.0) duration)))))

(defn timeline
  ([duration tracks] (timeline duration tracks {}))
  ([duration tracks {:keys [loop-start loop-end loop? playback-rate]
                     :or {loop-start 0 loop-end duration loop? false playback-rate 1.0}}]
   (when (or (not (pos? duration)) (< loop-start 0) (> loop-end duration) (>= loop-start loop-end))
     (throw (ex-info "invalid timeline or loop range" {:duration duration :loop-start loop-start :loop-end loop-end})))
   {:timeline/duration duration :timeline/tracks (vec tracks) :timeline/loop-start loop-start
    :timeline/loop-end loop-end :timeline/loop? loop? :timeline/playback-rate playback-rate}))
(defn evaluate [timeline time] (into {} (map (fn [t] [(:track/target t) (sample t time)]) (:timeline/tracks timeline))))

(defn playback-time [timeline elapsed]
  (let [scaled (* elapsed (:timeline/playback-rate timeline 1.0))
        start (:timeline/loop-start timeline 0) end (:timeline/loop-end timeline (:timeline/duration timeline))]
    (if (:timeline/loop? timeline)
      (+ start (mod (- scaled start) (- end start)))
      (max 0 (min (:timeline/duration timeline) scaled)))))
(defn evaluate-playback [timeline elapsed] (evaluate timeline (playback-time timeline elapsed)))

(defn auto-tangents
  "Calculate Catmull-Rom-style scalar slopes for a track, preserving key IDs."
  [{:track/keys [target keyframes]}]
  (track target
         (mapv (fn [i k]
                 (let [a (nth keyframes (max 0 (dec i))) b (nth keyframes (min (dec (count keyframes)) (inc i)))
                       dt (- (:keyframe/time b) (:keyframe/time a))
                       slope (if (zero? dt) 0.0 (/ (- (:keyframe/value b) (:keyframe/value a)) dt))]
                   (assoc k :keyframe/tangent-in slope :keyframe/tangent-out slope)))
               (range (count keyframes)) keyframes)))

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
