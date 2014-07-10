;;==================================================================;;
;; Joshua Taylor
;; December 7, 2009
;; tayloj@cs.rpi.edu
;;==================================================================;;

(in-package #:mobile-ambients-user)

;;; Utilities

;; Semaphores

(defun semaphore (s)
  (amb s))

(defun acquire (s P)
  (open s P))

(defun release (s)
  (amb s))

;;; Dining Philosophers

(defun chopstick (c)
  (semaphore c))

;;; Philosophers

(defun philosopher (l r)
  "The essential philosopher.  Later philosophers also have an id, and
give output on their progress."
  (acquire l
    (acquire r
      (par (release l)
           (release r)
           (fun (philosopher l r))))))

;; Part 0

(defun philosopher-0 (n l r)
  (flet ((msg (message) (format t "~&~A: ~A" n message)))
    (acquire l
      (fun (msg "got left")
        (acquire r
          (fun (msg "got right")
            (par (release l)
                 (release r)
                 (fun (philosopher-0 n l r)))))))))

(defun dining-philosophers-0 (n)
  (labels ((set-table (pn c0 l)
             (if (= pn n) (philosopher-0 n l c0)
               (new (r)
                 (par (chopstick r)
                      (philosopher-0 pn l r)
                      (fun (set-table (1+ pn) c0 r)))))))
    (run-process
     (new (c)
       (par (chopstick c)
            (fun (set-table 1 c c))))
     nil)))

;; Part 1

(defun bouncer (b n)
  "Bouncer spawns n semaphores, all named b."
  (if (= 1 n) (amb b)
    (par (amb b)
         (fun (bouncer b (1- n))))))

(defun get-seat (b P)
  (acquire b P))

(defun give-seat (b)
  (release b))

(defun philosopher-1 (n b l r)
  "This philosophers gets a seat before reaching for any chopsticks,
and releases the seat at the same time the chopsticks are released."
  (flet ((msg (message) (format t "~&~A: ~A" n message)))
    (get-seat b
      (fun (msg "seated")
        (acquire l
          (fun (msg "got left")
            (acquire r
              (fun (msg "got right")
                (par (release l)
                     (release r)
                     (fun (msg "standing")
                       (give-seat b))
                     (fun (philosopher-1 n b l r)))))))))))

(defun dining-philosophers-1 (n)
  (labels ((set-table (pn b c0 l)
             (if (= pn n) (philosopher-1 n b l c0)
               (new (r)
                 (par (chopstick r)
                      (philosopher-1 pn b l r)
                      (fun (set-table (1+ pn) b c0 r)))))))
    (run-process
     (new (c b)
       (par (bouncer b (1- n)) 
            (chopstick c)
            (fun (set-table 1 b c c))))
     nil)))

;; Other Possible Extension

(defun dining-philosophers-3 (n)
  "Another solution to the dining philosophers uses the same
philosopher behavior as the first, but the last philosopher to sit
down reaches for his right chopstick first, and then the left."
  (labels ((set-table (pn c0 l)
             (if (= pn n) (philosopher-0 n c0 l)
               (new (r)
                 (par (chopstick r)
                      (philosopher-0 pn l r)
                      (fun (set-table (1+ pn) c0 r)))))))
    (run-process
     (new (c)
       (par (chopstick c)
            (fun (set-table 1 c c))))
     nil)))
