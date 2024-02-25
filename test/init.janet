# Add some test
(use spork/test)
(import /app/init)

(use spork/test spork/http gp/data/schema)
(import /app)

(start-suite)
(assert-docs "/app/init")
(end-suite)
(def host-port "localhost:6666")

(defn url [&opt path] (string "http://" host-port path))

(def success? (??? {:status (?eq 200)}))

(defn success-has?
  [& parts]
  (??? {:status (?eq 200)
        :body (?find ;parts)}))

(defn success-has-not?
  [part]
  (??? {:status (?eq 200)
        :body (complement (?find part))}))

(defn redirect?
  [location]
  (??? {:status (?eq 303)
        :headers (?deep-eq @{"content-length" "0" "location" location})}))

(def test-image "./test/test-store")
(def test-image-file (string test-image ".jimage"))
(if (os/stat test-image-file) (os/rm test-image-file))

(start-suite :server)
(assert (ev/spawn
          (app/main "server"
                    @{:http host-port
                      :log false :static false
                      :image test-image
                      :debug (dyn *debug*)})))

(ev/sleep 0.00001) # Settle the server
(assert ((success-has? "textarea" "presentation")
          (request "GET" (url))))

(assert ((redirect? "/")
          (request "POST" (url)
                   :headers {"Content-Type"
                             "application/x-www-form-urlencoded"}
                   :body (slurp "./test/create-presentation-req"))))

(assert ((success-has? "CULS 2024" "Chapters" "The Intro" "3 slides" "edit" "start")
          (request "GET" (url))))

(assert ((success-has? "textarea" "presentation" "CULS 2024")
          (request "GET" (url "/edit"))))

(assert ((redirect? "/")
          (request "POST" (url)
                   :headers {"Content-Type"
                             "application/x-www-form-urlencoded"}
                   :body (slurp "./test/create-presentation-req"))))

(assert ((redirect? "/presentation")
          (request "GET" (url "/start"))))

(assert ((success-has? "<html" "Backend CULS 2024" "previous-slide" "next-slide"
                       "Future Backend Development")
          (request "GET" (url "/presentation"))))

(assert (success?
          (request "GET" (url "/next-slide"))))

(assert ((success-has? "Good Old Friends")
          (request "GET" (url "/slide"))))

(assert ((success-has? "button")
          (request "GET" (url "/navigation"))))

(assert (success?
          (request "GET" (url "/next-chapter"))))

(assert (success?
          (request "GET" (url "/previous-chapter"))))

(end-suite)
(os/exit 0)
