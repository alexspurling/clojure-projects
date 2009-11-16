(import '(javax.swing JFrame JPanel)
        '(java.awt Color Dimension Graphics)
        '(java.awt.image BufferedImage)
        '(java.awt.image WritableRaster))

(defn mandelformula [x0 y0 max-iters]
  "Applies the mandelbrot formula until max-iters iterations
  are reached, or the magnitude of Z exceeds 2"
  (let [x0 (double x0)
        y0 (double y0)
        max-iters (int max-iters)]
    (loop [x (double x0)
           y (double y0)
           n (int 0)]
      (if (== n max-iters)
        n
        (let [mag (+ (* x x) (* y y))]
          (if (>= mag (double 4))
            n
            (let [new-x (+ x0 (- (* x x) (* y y)))
                  new-y (+ y0 (* (double 2) (* x y)))]
            (recur new-x new-y (inc n)))))))))

;; Colours used to draw the set
(def *grad-colour-a* [255, 255, 0]) ;yellow
(def *grad-colour-b* [0, 0, 255]) ;blue
(def *set-colour* [0, 0, 0]) ;black

(defn grad-colour
  "Returns the colour that is the given fraction of the way between
  the first and second colours given. Returns as a vector of three
  integers between 0 and 255."
  [colA colB frac]
  (vec (map #(+ (* frac (- %2 %1)) %1) colA colB)))

(defn iter-colour 
  "Returns the colour needed to paint a point with the given number
  of iterations"
  [num-iters max-iters]
  (grad-colour *grad-colour-a* *grad-colour-b*
    (/ (double num-iters) max-iters)))

(defn coord-colour
  "Returns a colour for which to draw the given coordinate. If the coordinate
  is within the mandelbrot set, black is returned. Otherwise, a colour within
  a gradient is given based on the number of iterations of the mandelbrot set
  that have been evaluated."
  [[xcoord ycoord] max-iters]
  (let [num-iters (mandelformula xcoord ycoord max-iters)]
    (if (= max-iters num-iters)
      *set-colour*
      (iter-colour num-iters max-iters))))

(defn get-coord 
  "Returns the coordinates of the given pixel in the complex plane"
  [x y xstart ystart xsize ysize width height]
  [(+ xstart (* (/ x width) xsize))
   (+ ystart (* (/ y height) ysize))])

(defn get-pixels [width height]
  "Returns a sequence of vectors representing pixels in a grid of
  the given width and height"
  (for [y (range height) x (range width)]
     [x y]))

(defn render [xstart ystart xsize ysize width height max-iters #^WritableRaster wr]
  (doseq [pixel (get-pixels width height)]
    (let [[x y] pixel]
      (.setPixel wr (int x) (int y)
        (int-array
          (coord-colour
            (get-coord (double x) (double y) xstart ystart xsize ysize width height)
            max-iters))))))


(defn get-img [width height]
  (BufferedImage. width height (BufferedImage/TYPE_INT_RGB)))

(defn get-panel [width height img]
  (proxy [JPanel] [] (paint [g] (.drawImage g img 0 0 (Color/red) nil))))

(defn construct-frame [width height panel]
  "Creates and displays a JFrame of the given dimensions with
  the panel added to it"
  (let [frame (JFrame.)]
      (.setPreferredSize panel (Dimension. width height))
      (doto frame
        (.add panel)
        .pack
        (.setLocationRelativeTo nil)
        .show)))

(defn mandelbrot [xstart ystart xsize ysize width height max-iters]
  "Returns a function to render the mandelbrot set with the
  given parameters on a frame"
  (let [img (get-img width height)
        panel (get-panel width height img)
        wr (.getRaster img)]
    (construct-frame width height panel)
    (fn []
      (do
        (render xstart ystart xsize ysize width height max-iters wr)
        (.repaint panel)))))

(def my-mandelbrot (mandelbrot -2 -1.25 3 2.5 600 500 50))

(future (my-mandelbrot))