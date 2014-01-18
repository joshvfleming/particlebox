(ns physim.core
  (:require [THREE :as THREE]))

(def particle-radius 20)
(def particle-diameter (* 2 particle-radius))
(def particle-count 10)
(def gravity-multiplier 200)
(def speed-limit 5)
(def initial-speed 0)

(def scene-width window/innerWidth)
(def scene-height window/innerHeight)

(def particles (atom []))
(def particle-meshes (atom []))
(def collided-particles (atom {}))

(defn rand-x
  []
  (- (rand-int scene-width)
     (/ scene-width 2)))

(defn rand-y
  []
  (- (rand-int scene-height)
     (/ scene-height 2)))

(defn generate-particles
  [n]
  (for [i (range n)]
    { :pos [(rand-x) (rand-y) (rand-y)]
      :vel [(/ (* initial-speed (rand-x)) scene-width)
            (/ (* initial-speed (rand-y)) scene-width)
            (/ (* initial-speed (rand-y)) scene-width)] }))

(defn set-position!
  [mesh [x y z]]
  (set! (.-x (.-position mesh)) x)
  (set! (.-y (.-position mesh)) y)
  (set! (.-z (.-position mesh)) z))

(defn rotate-angles!
  [mesh [x y z]]
  (set! (.-x (.-rotation mesh)) (+ (.-x (.-rotation mesh)) x))
  (set! (.-y (.-rotation mesh)) (+ (.-y (.-rotation mesh)) y))
  (set! (.-z (.-rotation mesh)) (+ (.-z (.-rotation mesh)) z)))

(defn mesh-from-particle
  [p]
  (let [geometry (THREE.SphereGeometry.
                  particle-radius particle-radius particle-radius)
        material (THREE.MeshNormalMaterial.)
        mesh (THREE.Mesh. geometry material)]
    (set-position! mesh (:pos p))
    mesh))

(defn init-particles!
  [ps]
  (reset! particles ps)
  (reset! particle-meshes (map mesh-from-particle ps)))

(defn make-renderer!
  [meshes]
  (let [camera (THREE.PerspectiveCamera.
                50 (/ scene-width
                      scene-height) 1 10000)
        scene (THREE.Scene.)
        renderer (THREE.CanvasRenderer.)]
    (set! (.-z (.-position camera)) 1000)
    (doseq [m meshes]
      (.add scene m))
    (.setSize renderer scene-width scene-height)
    (.appendChild (.-body js/document) (.-domElement renderer))

    (fn []
      (.render renderer scene camera))))

(defn magnitude
  [v]
  (Math/sqrt (apply + (map #(* % %) v))))

(defn calc-accel
  [p all]
  (reduce (fn [a n]
            (let [dir (map - (:pos n) p)
                  mag (magnitude dir)
                  dir (map #(/ % mag) dir)]
              (map + a
                   (if (> mag 0)
                     (map #(* % (/ gravity-multiplier
                                   (* mag mag))) dir)
                     [0 0 0]))))
          [0 0 0]
          all))

(defn move
  [p v all]
  (let [a (calc-accel p all)
        v (map + v a)
        magv (magnitude v)
        v (if (> magv speed-limit)
            (map #(* (/ % magv) speed-limit) v)
            v)
        p (map + p v)]
    [p v a]))

(defn collided?
  [a b]
  (< (magnitude (map - a b)) particle-diameter))

(defn handle-collisions!
  []
  (let [n (count @particles)]
    (doseq [a (range n)
            b (range n)]
      (let [pa (nth @particles a)
            pb (nth @particles b)
            abdir (map - (:pos pa) (:pos pb))
            dist (magnitude abdir)
            abdir (map #(/ % dist) abdir)
            bounce (map #(* % (- particle-diameter dist)) abdir)]
        (when (and (not= a b) (collided? (:pos pa) (:pos pb)))
          (do
            (reset! particles (assoc @particles a (assoc pa :pos (map + (:pos pa) bounce) :vel (map + (:vel pa) (map #(* % (magnitude (:vel pa))) abdir)))))
            (reset! particles (assoc @particles b (assoc pb :pos (map - (:pos pb) bounce) :vel (map - (:vel pb) (map #(* % (magnitude (:vel pb))) abdir)))))))))))

(defn motion-tick!
  []
  (doseq [i (range (count @particles))]
    (let [p (nth @particles i)
          [p v a] (move (:pos p) (:vel p) @particles)]
      (reset! particles
              (assoc (vec @particles) i {:pos p :vel v})))))

(init-particles! (generate-particles particle-count))
(def render-scene (make-renderer! @particle-meshes))  
(defn render
  []
  (motion-tick!)
  (handle-collisions!)

  (doseq [i (range (count @particles))]
    (let [p (nth @particles i)
          m (nth @particle-meshes i)]
      ;; static rotation for blinginess
      (rotate-angles! m [0 2 0])
      (set-position! m (:pos p))))

  (render-scene))
 
(defn animate
  []
  (.requestAnimationFrame js/window animate)
  (render))

(animate)
