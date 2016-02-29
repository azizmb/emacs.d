(defun my/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar my/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around my/build-require-times (feature &optional filename noerror) activate)
  "Note in `my/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (my/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'my/require-times
                       (cons feature time)
                       t))))))



(provide 'init-benchmarking)
