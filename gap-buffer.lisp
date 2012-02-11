(in-package #:cl-gap-buffer)

(defparameter *gap-buffer-growth* 64
  "Positive integer specifying how much the buffer should grow if the
  gap closes.")

(defstruct (gap-buffer (:conc-name buf.)
                       (:print-function gap-buffer-print-function)
                       (:constructor make-raw-gap-buffer))
  data
  gap-start
  gap-end)

(defun gap-buffer-components (buf)
  "Return the string before the gap and after the gap in the gap
buffer BUF."
  (let ((data (buf.data buf)))
    (values (subseq data 0 (buf.gap-start buf))
            (subseq data (buf.gap-end buf) (length data)))))

(defun gap-buffer-print-function (buf stream depth)
  (declare (ignore depth))
  (print-unreadable-object (buf stream :type t :identity t)
    (when *print-array*
      (multiple-value-bind (start end) (gap-buffer-components buf)
        (format stream "~S+~A+~S"
                start
                (gap-buffer-gap-length buf)
                end)))))

(defun make-gap-buffer (&optional length)
  "Make a new gap buffer of size LENGTH."
  (make-raw-gap-buffer :data (make-array (or length *gap-buffer-growth*)
                                         :adjustable t
                                         :element-type 'character
                                         :initial-element #\_)
                       :gap-start 0
                       :gap-end (or length *gap-buffer-growth*)))

(defun gap-buffer-clear (buf)
  "Clear the contents of the gap buffer BUF."
  (setf (buf.gap-start buf) 0
        (buf.gap-end buf)   (length (buf.data buf)))
  buf)

(defun gap-buffer-length (buf)
  "Find the length of the gap buffer BUF. Analogous to LENGTH."
  (+ (buf.gap-start buf)
     (- (length (buf.data buf))
        (buf.gap-end buf))))

(defun gap-buffer-string (buf)
  "Format a gap buffer BUF into a string."
  (multiple-value-bind (start end) (gap-buffer-components buf)
    (concatenate 'string start end)))

;;; DOCUMENTATION: Compute the length of the gap of the gap buffer.
(defun gap-buffer-gap-length (buf)
  (- (buf.gap-end buf) (buf.gap-start buf)))

(defun gap-buffer-insert-char (buf char)
  "Insert a character CHAR into a gap buffer BUF at the gap, resizing
if needed."
  (when (zerop (gap-buffer-gap-length buf))
    (let ((new-data (make-array (+ *gap-buffer-growth* (length (buf.data buf)))
                                :adjustable t
                                :element-type 'character)))
      (setf (subseq new-data 0 (buf.gap-start buf))
            (subseq (buf.data buf) 0 (buf.gap-start buf))
            
            (subseq new-data
                    (+ *gap-buffer-growth* (buf.gap-end buf))
                    (+ *gap-buffer-growth* (length (buf.data buf))))
            (subseq (buf.data buf) (buf.gap-end buf) (length (buf.data buf)))
            
            (buf.data buf) new-data
            
            (buf.gap-end buf) (+ *gap-buffer-growth* (buf.gap-end buf)))))
  (setf (aref (buf.data buf) (buf.gap-start buf))
        char)
  (incf (buf.gap-start buf))
  buf)

;;; XXX: Improve from O(n) to O(1) time complexity.
(defun gap-buffer-insert-string (buf string)
  "Insert a string STRING into the gap buffer BUF."
  (loop
    :for c :across string
    :do (gap-buffer-insert-char buf c)
    :finally (return buf)))

(defun gap-buffer-move-left (buf)
  "Move the gap left in the gap buffer BUF."
  (unless (zerop (buf.gap-start buf))
    (setf (aref (buf.data buf) (1- (buf.gap-end buf)))
          (aref (buf.data buf) (1- (buf.gap-start buf))))
    
    (decf (buf.gap-start buf))
    (decf (buf.gap-end buf)))
  buf)

(defun gap-buffer-move-right (buf)
  "Move the gap to the right in the gap buffer BUF."
  (unless (=  (length (buf.data buf)) (buf.gap-end buf))
    (setf (aref (buf.data buf) (buf.gap-start buf))
          (aref (buf.data buf) (buf.gap-end buf)))
    
    (incf (buf.gap-start buf))
    (incf (buf.gap-end buf)))
  buf)

(defun gap-buffer-backspace (buf)
  "Backward-delete a character in the gap buffer BUF."
  (unless (zerop (buf.gap-start buf))
    (decf (buf.gap-start buf)))
  buf)

(defun gap-buffer-delete (buf)
  "Forward-delete a character in the gap buffer BUF. "
  (unless (= (length (buf.data buf)) (buf.gap-end buf))
    (incf (buf.gap-end buf)))
  buf)
