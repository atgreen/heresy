;;; Copyright (c) 2007, Matthew Lamari (matt.lamari@gmail.com).  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


; (load "c:/prog/lisp/common/stdutils.lisp")

(in-package :heresy)



(defun composed (&rest functions)
  (if functions
      (let (reversed last)
        (loop for remainder on functions do
              (if (cdr remainder)
                  (push (car remainder) reversed)
                (setq last (car remainder))))
        (lambda (&rest args)
          (let ((result (apply last args)))
            (loop for elt in reversed do (setq result (funcall elt result)))
            result)))
    #'identity))

(defun curried (function &rest largs)
  (assert (functionp function))
  (lambda (&rest rargs)
    (apply function (append largs rargs))))

(defun rcurried (function &rest rargs)
  (assert (functionp function))
  (lambda (&rest largs)
    (apply function (append largs rargs))))

(defclass unresolved () ((call :initarg :call :accessor get-call)))

(defmacro unresolved (form) `(make-instance 'unresolved :call (lambda () ,form)))
(defun unresolved-call (call) (unless (functionp call) (print call)) (make-instance 'unresolved :call call))
(defmacro resolved (form) (let ((temp-sym (gensym))) `(let ((,temp-sym ,form)) (loop while (typep ,temp-sym 'unresolved) do (setq ,temp-sym (funcall (get-call ,temp-sym)))) ,temp-sym)))


(defclass traversal-result () ((value :initarg :value :accessor get-value) (next :initarg :next :accessor get-next)))
(defun traversal-result (value next)
  (make-instance 'traversal-result :value value :next next))

(defstruct traversal-link
  get-link
  get-link-with-tail-override)

(defmacro get-traversal-result (traversal-link)
  `(funcall (traversal-link-get-link (resolved ,traversal-link))))

(defparameter **standard-terminating-end-call**
  (make-traversal-link
   :get-link (lambda () (traversal-result nil nil))
   :get-link-with-tail-override (lambda (end-call) (unresolved (get-traversal-result end-call)))))

#|
(defmacro assured-traversal-result (&body body)
  (let ((result-sym (gensym)))
    `(let ((,result-sym (resolved (progn ,@body))))
       (when (not (typep ,result-sym 'traversal-result)) (error (format nil "Not a Traversal-result:  ~S" ',body)))
       ,result-sym)))
|#

(defmacro standard-traversal-link ((build-func-sym (end-call-sym &rest build-params) &body body) (&rest build-actual-params))
  `(labels ((,build-func-sym (,end-call-sym ,@build-params)
              (make-traversal-link
               :get-link (lambda () ,@body)
               :get-link-with-tail-override (lambda (,end-call-sym) ,@body))))
     (,build-func-sym **standard-terminating-end-call** ,@build-actual-params)))

(defmacro fixed-traversal-link (value next)
  (let ((end-call-sym (gensym))
        (next-sym (gensym)))
    `(make-traversal-link
      :get-link (lambda () (traversal-result ,value ,next))
      :get-link-with-tail-override (lambda (,end-call-sym)
                                     (let ((,next-sym (resolved ,next)))
                                       (if ,next-sym
                                           (traversal-result ,value (unresolved (funcall (traversal-link-get-link-with-tail-override ,next-sym) ,end-call-sym)))
                                         (unresolved (get-traversal-result ,end-call-sym))))))))



(defmacro fixed-traversal-link-from-result-form (traversal-result-form)
  (let ((end-call-sym (gensym))
        (next-sym (gensym))
        (result-sym (gensym))
        (v-sym (gensym))
        (n-sym (gensym)))
    `(make-traversal-link
      :get-link (lambda () ,traversal-result-form)
      :get-link-with-tail-override (lambda (,end-call-sym)
                                     (let* ((,result-sym ,traversal-result-form)
                                            (,next-sym (resolved (get-next ,result-sym))))
                                       (if ,next-sym
                                           (traversal-result
                                            (get-value ,result-sym)
                                            (unresolved
                                             (with-traversal-result
                                              (,v-sym ,n-sym)
                                              (funcall (traversal-link-get-link-with-tail-override ,next-sym) ,end-call-sym)
                                              (fixed-traversal-link ,v-sym ,n-sym))))
                                         (unresolved (get-traversal-result ,end-call-sym))))))))



(defmacro get-traversal-result-new-end-call (traversal-link new-end-call)
  `(funcall (traversal-link-get-link-with-tail-override (resolved ,traversal-link)) ,new-end-call))

(defmacro deferred-traversal-link-from-call-maker (call-maker-form)
  (let ((end-call-sym (gensym)))
    `(make-traversal-link
      :get-link (lambda () (get-traversal-result ,call-maker-form))
      :get-link-with-tail-override (lambda (,end-call-sym) (get-traversal-result-new-end-call ,call-maker-form ,end-call-sym)))))



(defun confirmed-traversal-result (val)
  (assert (typep val 'traversal-result))
  val)

(defmacro with-traversal-result ((val-sym next-sym) form &body body)
  (let ((sym (gensym)))
    `(with-slots ((,val-sym value) (,next-sym next))
;         (let ((,sym ,form)) (assert (typep ,sym 'traversal-result)) ,sym)
;         (confirmed-traversal-result ,form)
         (let ((,sym (resolved ,form)))
           (when (not (typep ,sym 'traversal-result))
             (print (list "bad type for traversal type " ,sym ',body))
             )
           ,sym)
       ,@body)))


(defclass lazy-list () ((call-for-first :initarg :call-for-first :accessor get-call-for-first)))

#|
(defmethod get-call-for-first ((list lazy-list) (call-for-end function))
  (resolved (funcall (get-call-for-first-maker list) call-for-end)))
|#

(defclass lazy-list-under-cdrs (lazy-list)
  ((underlying-call-for-first :initarg :underlying-call-for-first :accessor get-underlying-call-for-first)
   (cdr-count :initarg :cdr-count :accessor get-cdr-count)))

(defclass lazy-list-with-some-persistence (lazy-list) ())

(defclass lazy-list-with-persistence (lazy-list-with-some-persistence) ())

(defclass lazy-list-read-point-based (lazy-list-with-some-persistence) ((read-point :initarg :read-point :accessor get-read-point)))

(defclass lazy-list-known-empty (lazy-list-with-persistence) ())

(defclass lazy-list-list-based (lazy-list-with-persistence)
  ((list-head :initarg :list-head :accessor get-list-head)))

(defclass lazy-list-pair-based (lazy-list-with-persistence)
  ((cons :initarg :cons :accessor get-cons)))


(defun make-instance-2 (type &rest params)
  (when (eql type 'lazy-list-read-point-based)
      (when (not (getf params :read-point))
        (error "No read-point")))
  (apply #'make-instance type params))


(defun lazy-list-from-call (call)
  (assert (typep call 'traversal-link))
  (make-instance-2 'lazy-list :call-for-first call))

(defun lazy-list-from-traversal-link (traversal-link)
  (assert (typep traveral-link 'traversal-link))
  (make-instance-2 'lazy-list :call-for-first traversal-link))




(defparameter **in-lazy-mode** nil)


(defun in-lazy-mode ()
  (declare (special **in-lazy-mode**))
  **in-lazy-mode**)


(defmacro if-lazy-eager (if-lazy if-strict)
  `(if (in-lazy-mode)
       ,if-lazy
     ,if-strict))

(defmacro lazy (&body body)
  "Enters a \"lazy\" context - calls to functions such as tail/ defer traversal.
This context uses a special variable, and extends into sub-calls until overridden."
  `(let ((**in-lazy-mode** t))
     (declare (special **in-lazy-mode**))
     ,@body))

(defmacro eager (&body body)
  "Enters an \"eager\" context - calls to functions such as tail/ do traversal before returning.
This context uses a special variable, and extends into sub-calls until overridden."
  `(let ((**in-lazy-mode** nil))
     (declare (special **in-lazy-mode**))
     ,@body))



(defparameter **respecting-thread-safety** nil)

(defstruct read-point
  rp-value
  rp-next
  rp-lock)

(defstruct read-point-value-resolver
  run)

(defstruct read-point-next-resolver
  run
  get-call)

(defmacro respecting-lock-if-present ((lock) &body body)
  `(if ,lock
      (bordeaux-threads::with-lock-held (,lock) ,@body)
     (progn ,@body)))

(defmacro respecting-read-point-lock ((read-point) &body body)
  (let ((lock-sym (gensym)))
    `(let ((,lock-sym (read-point-rp-lock ,read-point)))
       (respecting-lock-if-present (,lock-sym) ,@body))))

(defmacro assure-readpoint-value-resolved (read-point)
  (let ((sym (gensym)))
    `(let ((,sym (read-point-rp-value ,read-point)))
       (when (typep ,sym 'read-point-value-resolver)
         (funcall (read-point-value-resolver-run ,sym))))))

(defmacro assure-readpoint-next-resolved (read-point)
  (let ((sym (gensym)))
    `(let ((,sym (read-point-rp-next ,read-point)))
       (when (typep ,sym 'read-point-next-resolver)
         (funcall (read-point-next-resolver-run ,sym))))))

(defun read-point-value (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-value-resolved read-point)
   (read-point-rp-value read-point)))

(defun read-point-at-end (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-next-resolved read-point)
   (not (read-point-rp-next read-point))))


(defun read-point-advanced (read-point)
  (assert (typep read-point 'read-point))
  (respecting-read-point-lock
   (read-point)
   (assure-readpoint-next-resolved read-point)
   (read-point-rp-next read-point)))


(defun read-point-from-call (call &optional (lock (when **respecting-thread-safety** (bordeaux-threads:make-lock))))
  (let ((read-point nil))
    (setq read-point
          (make-read-point
           :rp-lock lock
           :rp-value (make-read-point-value-resolver
                      :run
                      (lambda ()
                        (respecting-lock-if-present
                         (lock)
                         (with-traversal-result
                          (value next)
                          (resolved (get-traversal-result call))
                          (setf (read-point-rp-value read-point) (resolved value))
                          (setf (read-point-rp-next read-point)
                                (make-read-point-next-resolver
                                 :run
                                 (lambda ()
                                   (respecting-lock-if-present
                                    (lock)
                                    (setf (read-point-rp-next read-point)
                                          (let ((next (resolved next)))
                                            (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock))))))))
                                 :get-call
                                 (lambda ()
                                   (respecting-lock-if-present
                                    (lock)
                                    (let ((next (resolved next)))
                                      (setf (read-point-rp-next read-point)
                                            (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                                      next)))))))))
           :rp-next (make-read-point-next-resolver
                     :run
                     (lambda ()

                       (respecting-lock-if-present
                        (lock)
                        (with-traversal-result
                         (value next)
                         (resolved (get-traversal-result call))
                         (let ((next (resolved next)))
                           (setf (read-point-rp-next read-point) (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                           (setf (read-point-rp-value read-point)
                                 (when next
                                   (if (typep value 'unresolved)
                                       (make-read-point-value-resolver
                                        :run
                                        (lambda ()
                                          (respecting-lock-if-present
                                           (lock)
                                           (setf (read-point-rp-value read-point) (resolved value)))))
                                     value)))))))
                     :get-call
                     (lambda ()
                       (respecting-lock-if-present
                        (lock)
                        (with-traversal-result
                         (value next)
                         (resolved (get-traversal-result call))
                         (let ((next (resolved next)))
                           (setf (read-point-rp-next read-point) (when next (read-point-from-call next (when lock (bordeaux-threads:make-lock)))))
                           (setf (read-point-rp-value read-point)
                                 (when next
                                   (if (typep value 'unresolved)
                                       (make-read-point-value-resolver
                                        :run
                                        (lambda ()
                                          (respecting-lock-if-present
                                           (lock)
                                           (setf (read-point-rp-value read-point) (resolved value)))))
                                     value)))
                           next)))))))
    read-point))

(defun read-point-built (list)
  (etypecase list
    (lazy-list-read-point-based (get-read-point list))
    (lazy-list
     (read-point-from-call (get-call-for-first list)))))

(defun call-for-read-point-taken-to-end (read-point)
  (assert (typep read-point 'read-point))
  (standard-traversal-link
   (build (end-call read-point)
          (if (read-point-at-end read-point)
              (unresolved (get-traversal-result end-call))
            (let ((advanced (read-point-advanced read-point))) ; calc here to (potentially) advance value
              (let ((value (read-point-rp-value read-point)))
                (traversal-result
                 (if (typep value 'read-point-value-resolver)
                     (unresolved (read-point-value read-point))
                   value)
                 (build end-call advanced))))))
   (read-point)))

(defun call-to-detach-from-read-point (read-point)
  (assert (typep read-point 'read-point))
  (standard-traversal-link
   (build (end-call read-point)
          (let ((result
                 (respecting-read-point-lock
                  (read-point)
                  (let ((rp-next (read-point-rp-next read-point)))
                    (cond
                     ((null rp-next) (get-traversal-result end-call))
                     (t
                      (etypecase rp-next
                        (read-point (traversal-result (unresolved (read-point-value read-point)) (build end-call rp-next)))
                        (read-point-next-resolver
                         (unresolved
                           (let ((nexts-call (funcall (read-point-next-resolver-get-call rp-next))))
                             (if nexts-call
                                 (traversal-result
                                  (unresolved (read-point-value read-point))
                                  (with-traversal-result
                                   (val next)
                                   (get-traversal-result-new-end-call nexts-call end-call)
                                   (let ((next (resolved next)))
                                     (if next
                                         (fixed-traversal-link val next)
                                       end-call))))
                               (get-traversal-result end-call))))))))))))
            result))
   (read-point)))


(defun lazy-list-from-read-point (read-point)
  (make-instance-2 'lazy-list-read-point-based :call-for-first (call-for-read-point-taken-to-end read-point) :read-point read-point))


; Runs in block nil (return will break out)
(defmacro loop-over/ (symbol lazy-list &body body)
  (let ((current-sym (gensym)) (value-sym (gensym)) (next-sym (gensym)) (top-sym (gensym)))
    "Most trivial loop construct - loops a symbol across lazy-list running body.  Runs in block NIL."
    `(block nil
       (let ((,current-sym (get-call-for-first ,lazy-list)))
         (tagbody
          ,top-sym
          (with-traversal-result (,value-sym ,next-sym)
                                 (resolved (get-traversal-result ,current-sym))
                                 (let ((,next-sym (resolved ,next-sym))
                                       (,value-sym (resolved ,value-sym)))
                                   (when ,next-sym
                                     (let ((,symbol (resolved ,value-sym)))
                                       ,@body
                                       (setf ,current-sym ,next-sym)
                                       (go ,top-sym))))))))))

(defmethod print-object ((lazy-list lazy-list) stream)
;  (format stream "(LIST/ #|Known Type: ~S|#" (type-of lazy-list))
  (format stream "(LIST/")
  (loop-over/ elt lazy-list (format stream " ~S" elt))
  (format stream ")"))

(defmacro list-to-lazy-list-call (origin &key (terminator-generator (lambda (rest-sym) rest-sym)) (value-generator (lambda (rest-sym) `(car ,rest-sym))))
  (let* ((build-sym (gensym))
         (rest-sym (gensym))
         (end-call-sym (gensym)))
    `(standard-traversal-link
      (,build-sym (,end-call-sym ,rest-sym)
                  (if ,(funcall (eval terminator-generator) rest-sym)
                      (traversal-result ,(funcall (eval value-generator) rest-sym) (,build-sym ,end-call-sym (cdr ,rest-sym)))
                    (unresolved (get-traversal-result ,end-call-sym))))
      (,origin))))


(defun to-lazy-list (list)
  (etypecase list
    (lazy-list list)
    (list (if list
              (make-instance-2 'lazy-list-list-based :call-for-first (list-to-lazy-list-call list) :list-head list)
            (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))
    (vector
     (let ((length (length list)))
       (make-instance-2 'lazy-list-with-persistence
                        :call-for-first
                        (standard-traversal-link
                         (build (end-call current-index)
                         (if (eql current-index length)
                             (unresolved (get-traversal-result end-call))
                           (traversal-result
                            (aref list current-index)
                            (build end-call (1+ current-index)))))
                         (0)))))



    (array
     (let* ((dimensions (array-dimensions list))
            (array-rank (array-rank list)))
       (make-instance-2 'lazy-list-with-persistence
                        :call-for-first
                        (standard-traversal-link
                         (build (end-call current-dimension-head prior-coords current-coord)
                                (if (eq (car current-dimension-head) current-coord) ; test for end of axis.
                                    (unresolved (get-traversal-result end-call))
                                  (traversal-result
                                   (if (cdr current-dimension-head) ; Another axis.
                                       (lazy-list-from-call (build **standard-terminating-end-call** (cdr current-dimension-head) (append prior-coords (list current-coord)) 0))
                                     (apply #'aref list (append prior-coords (list current-coord))))
                                   (build end-call current-dimension-head prior-coords (1+ current-coord)))))
                         (dimensions nil 0)))))))


(defun memoized/ (list)
  "Caches list on first traversal (unless it's determined to already be implemented in terms of persistence)."
  (etypecase list
    (sequence (to-lazy-list list))
    (lazy-list-with-some-persistence list)
    (lazy-list (lazy-list-from-read-point (read-point-built list)))))


(defun list/ (&rest rest)
  "Lazy equivalent of CL's list function - returning a lazy-list (although one that has the parameter list at its core)."
  (if rest
      (make-instance-2 'lazy-list-list-based :call-for-first (list-to-lazy-list-call rest) :list-head rest)
    (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))


(defun iterate/ (from-previous element &optional (end-before-func (constantly nil)))
  "Haskell's iterate function - returns element, then (funcall from-previous element), etc. against result"
  (assert (functionp from-previous))
  (lazy-list-from-call
   (standard-traversal-link
    (get-val (end-call elt)
             (if (funcall end-before-func elt)
                 (unresolved (get-traversal-value end-call))
               (traversal-result
                elt
                (get-val end-call (funcall from-previous elt)))))
    (element))))


(defun iteratex/ (input-to-contribution initial-input)
  (assert (functionp input-to-contribution))
  (labels ((build (input)
             (lambda ()
               (block nil
                 (let ((current-input input))
                   (tagbody
                    top
                    (let ((contribution (funcall input-to-contribution current-input)))
                      (destructuring-bind (primary-result
                                           &key
                                           (emissions #| (list current-input) |# nil emissions-supplied-p)
                                           (emission nil emission-supplied-p)
                                           (exit-before nil)
                                           (exit-after nil)
                                           (next-input primary-result))
                          contribution
                        (assert (not (and emissions-supplied-p emission-supplied-p))) ; can *NOT* supply both
                        (cond
                         (exit-before (return (call-for-end)))
                         ((and emissions-supplied-p (null/ emissions)) (setq current-input next-input) (go top))
                         (t
                          (if emission-supplied-p
                              (return
                               (values emission
                                       (if exit-after
                                           #'call-for-end
                                         (build next-input))))
                            (labels ((build-for-read-point (read-point)
                                       (values (read-point-value read-point)
                                               (let ((advanced (read-point-advanced read-point)))
                                                 (if (read-point-at-end advanced)
                                                     (if exit-after
                                                         #'call-for-end
                                                       (build next-input))
                                                   (lambda () (build-for-read-point advanced)))))))
                              (return (build-for-read-point (read-point-built (to-lazy-list emissions))))))))))))))))
    (lazy-list-from-call (build initial-input))))

(defun to-list (list)
"Returns the proper list corresponding to the passed-in list designator - attempts to minimize work involved if list is a CL sequence
or a lazy-list based upon a fixed container"
  (etypecase list
    (lazy-list-list-based (get-list-head list))
    (lazy-list-pair-based (destructuring-bind (first . second) (get-cons list) (list first second)))
    (lazy-list
     (let ((result nil))
       (loop-over/ elt list (push elt result))
       (nreverse result)))
    (list list)
    (sequence (map 'list #'identity list))))

(defun to-array (list &rest array-params)
"Returns the array corresponding to the passed-in list designator - attempts minimize work involved if list is a CL sequence or lazy-list
based upon a fixed container"
  (etypecase list
    (sequence (apply #'make-array (length list) :initial-contents list array-params))
    (t
     (let ((result nil)
           (count 0))
       (loop-over/ elt (to-lazy-list list) (progn (push elt result) (incf count)))
       (apply #'make-array count :initial-contents (nreverse result) array-params)))))


(defun string-from-chars/ (chars-list)
"Returns a string from the supplied chars list"
  (typecase chars-list
    (string chars-list)
    (array (let ((result (make-string (length chars-list))))
             (loop for i from 0
                   for elt across chars-list do
                   (setf (aref result i) elt))
             result))
    (t
     (let ((result nil)
           (count 0))
       (loop-over/ elt (to-lazy-list chars-list) (progn (push elt result) (incf count)))
       (let ((string (make-string count)))
         (loop for index from (1- count) downto 0
               for elt in result do
               (setf (aref string index) elt))
         string)))))



(defmethod to-string ((chars-list sequence))
"Returns a string from the supplied chars list"
  (string-from-chars/ chars-list))


(defmethod to-string ((chars-list lazy-list))
"Returns a string from the supplied chars list"
  (string-from-chars/ chars-list))



(defun length/ (list)
  "Returns the length of the supplied list, evaluating the list to the end if lazy to measure its length."
  (typecase list
    (sequence (length list))
    (lazy-list-list-based (length (get-list-head list)))
    (lazy-list-pair-based 2)
    (lazy-list-known-empty 0)
    (t
     (let ((current (read-point-built (to-lazy-list list)))
           (len 0))
       (tagbody
        top
        (setq current (read-point-advanced current))
        (incf len)
        (if (not (read-point-at-end current)) (go top)))
       len))))


(defun cdr/ (list)
  "Equivalent of Haskell's tail function or CL's CDR - traverses in eager context, defers in lazy."
  (typecase list
    (lazy-list-known-empty list)
    (list (to-lazy-list (cdr list)))
    (lazy-list-list-based (to-lazy-list (cdr (get-list-head list))))
    (lazy-list
     (if-lazy-eager
      (typecase list
        (lazy-list-under-cdrs
         (let ((new-cdr-count (1+ (get-cdr-count list)))
               (underlying-call-for-first (get-underlying-call-for-first list)))
           (assert (> new-cdr-count 0))
           (make-instance-2 'lazy-list-under-cdrs
                            :call-for-first
                            (standard-traversal-link
                             (build (end-call)
                                (let ((current (resolved (with-traversal-result (value next) (resolved (get-traversal-result-new-end-call underlying-call-for-first end-call)) next))))
                                  (loop for i from 2 to new-cdr-count
                                        while current
                                        do
                                        (when current (with-traversal-result (value next) (resolved (get-traversal-result current)) (setq current (resolved next)))))
                                   (if current
                                       (get-traversal-result current)
                                     (traversal-result nil nil))))
                             ())
                            :cdr-count new-cdr-count
                            :underlying-call-for-first underlying-call-for-first)))
        (t
         (let ((call-for-first (get-call-for-first list)))
           (make-instance-2 'lazy-list-under-cdrs
                            :call-for-first
                            (standard-traversal-link
                             (build (end-call)
                                    (with-traversal-result
                                     (value next)
                                     (get-traversal-result-new-end-call call-for-first end-call)
                                     (unresolved
                                      (get-traversal-result next))))
                             ())
                            :cdr-count 1
                            :underlying-call-for-first call-for-first))))
      (typecase list
        (lazy-list-read-point-based
         (let ((read-point (get-read-point list)))
           (if (read-point-at-end read-point)
               (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)
             (let ((advanced (read-point-advanced read-point)))
               (make-instance-2 'lazy-list-read-point-based :call-for-first (call-for-read-point-taken-to-end advanced end-call) :read-point advanced)))))
        (t
         (make-instance-2
          (case (type-of list) ; tail keeps persistence
            ((lazy-list-with-persistence lazy-list-with-some-persistence) (type-of list))
            (t 'lazy-list))
          :call-for-first
          (with-traversal-result
           (value next)
           (get-traversal-result (get-call-for-first list))
           (resolved next)))))))
    (t (cdr/ (to-lazy-list list)))))



(defun tail/ (list)
  "Equivalent of Haskell's tail function (or cdr/ ) - traverses in eager context, defers in lazy."
  (cdr/ list))


(defmethod car/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (typecase list
    (list (car list))
    (lazy-list-list-based (car (get-list-head list)))
    (lazy-list
     (with-traversal-result (value next) (get-traversal-result (get-call-for-first list)) (resolved value)))
    (t (with-traversal-result (value next) (get-traversal-result (get-call-for-first (to-lazy-list list))) (resolved value)))))

(defun head/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (car/ list))

(defun first/ (list)
  "Equivalent of CL's car/first or Haskell's head - returns first value in list, or nil if list is empty"
  (car/ list))


(defmacro build-car-things ()
  `(progn
     ,@(loop for len from 2 to 5 collect
             `(progn
                ,@(labels ((build-it (bits-left val)
                             (if (zerop bits-left)
                                 (list nil #'identity)
                               (destructuring-bind (char-list func)
                                   (build-it (1- bits-left) (floor (/ val 2)))
                                 (if (evenp val)
                                     (list (cons #\a char-list) (lambda (elt) `(car/ ,(funcall func elt))))
                                   (list (cons #\d char-list) (lambda (elt) `(cdr/ ,(funcall func elt)))))))))
                    (loop for combo from 0 to (1- (expt 2 len)) collect
                          (destructuring-bind (char-list func)
                              (build-it len combo)
                            ; (format t ":~A" (concatenate 'string "c" (map 'string #'identity char-list) "r/"))
                            ;(terpri)
                            ;(force-output)
                            (labels ((make-core-string (char-list)
                                       (concatenate 'string "c" (map 'string #'identity char-list) "r"))
                                     (th (n) (case n (1 "1st") (2 "2nd") (3 "3rd") (t (format nil "~Ath" n)))))
                              `(defun ,(read-from-string (concatenate 'string (make-core-string char-list) "/"))
                                      (list-designator)
                                 ,(let ((a-count (length (remove-if-not (lambda (elt) (eql elt #\a)) char-list)))
                                        (d-count (length (remove-if-not (lambda (elt) (eql elt #\d)) char-list))))
                                    (cond
                                     ((zerop a-count)
                                      (format nil "Returns list-designator's list with ~A item skipped, as a lazy-list" d-count))
                                     ((= 1 a-count)
                                      (format nil "Returns ~A element in list-designator" (th (1+ d-count))))
                                     (t
                                      (let ((trailing-d-count (length (loop for char in (reverse char-list) while (eql #\d char) collect char))))
                                        (case trailing-d-count
                                          (0 (format nil "~A/ of first element in list-designator" (make-core-string (butlast char-list))))
                                          (t
                                           (format nil "~A/ of ~A element in list-designator" (make-core-string (butlast char-list (1+ trailing-d-count))) (th trailing-d-count))))))))
                                 ,(funcall func 'list-designator))))))))))
(build-car-things)



; returns head and tail as successive values
(defun head-tail/ (list)
  "Returns head and tail of list as successive values, and whether or not head is a valid value as the third result."
  (etypecase list
    (list (values (car list) (to-lazy-list (cdr list)) (consp list)))
    (lazy-list-list-based (destructuring-bind (head . tail) (get-list-head list) (values head (to-lazy-list tail) (consp (get-list-head list)))))
    (t (with-traversal-result (head tail-call) (resolved (get-traversal-result (get-call-for-first (to-lazy-list list))))
                              (let ((tail-call (resolved tail-call)))
                                (if tail-call
                                    (values (resolved head) (lazy-list-from-call tail-call))
                                  (values)))))))



(defun nthcdr/ (to-drop list)
  "Equivalent to CL's nthcdr or Haskell's drop - returns list with to-drop elements skipped - traversing at point of call in eager context,
deferring traversal in lazy context."
  (labels ((nthcdr-known-lazy-list/ (list)
             (if-lazy-eager
              (lazy-list-from-call
               (deferred-traversal-link-from-call-maker
                (standard-traversal-link
                 (build (end-call)
                        (let ((current (get-call-for-first list)))
                          (loop for i from 1 to to-drop while current do
                                (setq current (resolved (with-traversal-result (value next) (get-traversal-result current) (resolved next)))))
                          (if current
                              (get-traversal-result-new-end-call current end-call)
                            (get-traversal-result end-call))))
                 ())))
              (lazy-list-from-call
               (standard-traversal-link
                (build (end-call)
                       (let ((current (get-call-for-first list)))
                         (loop for i from 1 to to-drop while current do
                               (setq current (resolved (with-traversal-result (value next) (get-traversal-result current) (resolved next)))))
                         (if current
                             (get-traversal-result-new-end-call current end-call)
                           (get-traversal-result end-call))))
                ())))))
    (typecase list
      (list (to-lazy-list (nthcdr to-drop list)))
      (lazy-list-list-based (to-lazy-list (nthcdr to-drop (get-list-head list))))
      (lazy-list
       (nthcdr-known-lazy-list/ list))
      (t (nthcdr-known-lazy-list/ (to-lazy-list list))))))



(defun drop/ (to-drop list)
  "Equivalent to CL's nthcdr or Haskell's drop - returns list with to-drop elements skipped - traversing at point of call in eager context,
deferring traversal in lazy context."
  (nthcdr/ to-drop list))



(defun null/ (list)
  "Returns nil if list has contents, a value otherwise.  Will only traverse a single element for lazy-lists"
  (etypecase list
    (list (null list))
    (sequence (zerop (length list)))
    (lazy-list-list-based (null (get-list-head list)))
    (lazy-list-known-empty t)
    (lazy-list (with-traversal-result (value next) (resolved (get-traversal-result (get-call-for-first list))) (not (resolved next))))))

(defun non-null/ (list)
  "Not null/"
  (not (null/ list)))




(defun take/ (to-take list)
  "Returns a lazy-list of the first to-take elements from list.  Performance note:  The resulting lazy-list will tend to maintain a reference to the original list, convert to a static container
(via to-list of to-array) to break this link."
  (assert (integerp to-take))
  (if (zerop to-take)
      (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)
    (lazy-list-from-call
     (standard-traversal-link
      (build (end-call current-call num-left)
             (if current-call
                 (with-traversal-result
                  (value next)
                  (get-traversal-result current-call)
                  (let ((next (resolved next)))
                    (if next
                        (let ((new-num-left (1- num-left)))
                          (traversal-result value (build end-call (if (zerop new-num-left) nil next) new-num-left)))
                      (unresolved (get-traversal-result end-call)))))
               (unresolved (get-traversal-result end-call))))
      ((get-call-for-first (to-lazy-list list)) to-take)))))




(defun split-when---experimental/ (predicate list)
  "Returns lazy-lists for before-split-point, and split-point-and-after, as first and second value results"
  (labels ((get-split-result ()
             (labels ((split-for-proper-list (list)
                        (let ((current list)
                              (before-predicate-r nil))
                          (tagbody
                           top
                           (when current
                             (let ((val (car current)))
                               (when (not (funcall predicate val))
                                 (setq current (cdr current))
                                 (push val before-predicate-r)))))
                          (values (to-lazy-list (nreverse before-predicate-r)) (to-lazy-list current)))))
               (let* ((list (to-lazy-list list))
                      (current (read-point-built list))
                      (before-predicate-r nil))
                 (tagbody
                  top
                  (when (not (read-point-at-end current))
                    (let ((val (read-point-value current)))
                      (when (not (funcall predicate val))
                        (setq current (read-point-advanced current))
                        (push val before-predicate-r)
                        (go top)))))
                 (values
                  (to-lazy-list (nreverse before-predicate-r))
                  (typecase list
                    (lazy-list-read-point-based (lazy-list-from-read-point current))
                    (lazy-list-with-persistence (make-instance-2 'lazy-list-with-persistence :call-for-first (call-to-detach-from-read-point current)))
                    (t (make-instance-2 'lazy-list :call-for-first (call-to-detach-from-read-point current)))))))))
    (if-lazy-eager
     (let ((lock (make-thread-lock))
           (result-known nil)
           (result-first nil)
           (result-second nil))
       (labels ((ensure-result ()
                  (when (not result-known) (respecting-lock-if-present (lock) (when (not result-known) (multiple-value-setq (result-first result-second) (get-split-result)) (setq result-known t))))))
         (values
          (lazy-list-from-call (lambda () (ensure-result) (funcall (get-call-for-first result-first))))
          (lazy-list-from-call (lambda () (ensure-result) (funcall (get-call-for-first result-second)))))))
     (get-split-result))))



(defun intersperse/ (val list)
  "Equivalent of Haskell's intersperse function - returns a lazy-list of val interspersed between elements of list.  If list is of length 0 or 1, val does not appear."
  (let ((traversal-result (resolved (get-traversal-result (get-call-for-first (to-lazy-list list))))))
    (let ((next (resolved (get-next traversal-result))))
      (if next
          (lazy-list-from-call
           (standard-traversal-link
            (build (end-call current-value current-resolved-next)
                   (traversal-result
                    current-value
                    (fixed-traversal-link-from-result-form
                     (if current-resolved-next
                         (with-traversal-result
                          (value next)
                          (get-traversal-result current-resolved-next)
                          (let ((next (resolved next)))
                            (if next
                                (traversal-result
                                 val
                                 (build end-call value next))
                              (get-traversal-result end-call))))
                       (get-traversal-result end-call)))))
            ((get-value traversal-result) next)))
        (make-instance-2 'lazy-list-known-empty :call-for-first **standard-terminating-end-call**)))))


; returns 3 values - lazy-list to predicate true, lazy-list of remainder, and (lambda () (values value true-if-found)) at predicate true
(defun split-on-test/ (test list)
  (let* ((split-value-known nil)
         (known-split-value nil)
         (known-post-value-remainder nil)
         (rest-of-pre-predicate-read-point
          (read-point-from-call
           (standard-traversal-link
            (build (end-call current-read-point)
                       (if (read-point-at-end current-read-point)
                           (progn
                             (setq rest-of-pre-predicate-read-point (read-point-from-call **standard-terminating-end-call**))
                             (setq known-post-value-remainder (read-point-from-call **standard-terminating-end-call**))
                             (get-traversal-result end-call))
                         (let ((value (read-point-value current-read-point))
                               (advanced (read-point-advanced current-read-point)))
                           (if (funcall test value)
                               (progn
                                 (setq known-split-value value)
                                 (setq split-value-known t)
                                 (setq rest-of-pre-predicate-read-point (read-point-from-call **standard-terminating-end-call**))
                                 (setq known-post-value-remainder advanced)
                                 (get-traversal-result end-call))
                             (progn
                               (setq rest-of-pre-predicate-read-point advanced)
                               (traversal-result value (build end-call advanced)))))))
            ((read-point-built (to-lazy-list list)))))))
    (values
     (lazy-list-from-read-point rest-of-pre-predicate-read-point)
     (lazy-list-from-call
      (standard-traversal-link
       (build (end-call)
              (loop while (progn (read-point-at-end rest-of-pre-predicate-read-point) (not known-post-value-remainder)) do (setq rest-of-pre-predicate-read-point (read-point-advanced rest-of-pre-predicate-read-point)))
              (get-traversal-result-new-end-call (call-to-detach-from-read-point known-post-value-remainder) end-call))
       ()))
     (lambda ()
       (loop while (progn (read-point-at-end rest-of-pre-predicate-read-point) (not known-post-value-remainder)) do (setq rest-of-pre-predicate-read-point (read-point-advanced rest-of-pre-predicate-read-point)))
       (values known-split-value split-value-known)))))



(defun split-on-test-to-first-non-empty-before/ (test list)
  (multiple-value-bind (before after call)
      (split-on-test/ test list)
    (let ((before before)
          (call call)
          (after after))
      (let ((before-null (null/ before))
            (after-null (null/ after)))
        (loop while (and before-null (not after-null)) do
              (multiple-value-setq (before after call)
                  (split-on-test/ test after))
              (setq before-null (null/ before))
              (setq after-null (null/ after)))
        (multiple-value-bind (split-val split-val-present)
            (funcall call)
          (if (and before-null after-null (not split-val-present))
              (call-for-end)
            (values split-val split-val-present before before-null after after-null)))))))


; returns list of list, value, list, value, list, value, list where value = something that triggers test
(defun split-down-on-test/ (test list &key (keep-split-causing-elements nil) (keep-empty-non-split t) (process-split-causing-element #'identity) (process-non-split-causing-elements-list #'identity))
  (lazy-list-from-call
   (if keep-split-causing-elements
       (standard-traversal-link
        (build-keep (end-call list)
                    (multiple-value-bind (before after val-maker)
                        (split-on-test/ test list)
                      (multiple-value-bind (v exists)
                          (funcall val-maker)
                        (cond
                         ((and exists (null/ before) (not keep-empty-non-split)) (traversal-result (funcall process-split-causing-element v) (build-keep end-call after)))
                         (exists (traversal-result
                                  (funcall process-non-split-causing-elements-list before)
                                  (fixed-traversal-link (funcall process-split-causing-element v) (build-keep end-call after))))
                         ((not (null/ before)) (traversal-result
                                                (funcall process-non-split-causing-elements-list before)
                                                end-call))
                         (t (unresolved (get-traversal-result end-call)))))))
        (list))
     (standard-traversal-link
      (build-no-keep (end-call list)
               ; Find first non-null "before"
                     ; (progn (to-list list) (print "list good 1"))
                     (multiple-value-bind (before after call)
                         (split-on-test/ test list)
                       (let ((before before)
                             (call call)
                             (after after))
                         (let ((before-null (null/ before))
                               (after-null (null/ after)))
                           (unless keep-empty-non-split
                             (loop while (and before-null (not after-null)) do
                                   (multiple-value-setq (before after call)
                                       (split-on-test/ test after))
                                   (setq before-null (null/ before))
                                   (setq after-null (null/ after))))
                           (if (and before-null after-null)
                               (unresolved (get-traversal-result end-call))
                             (progn
                               (traversal-result
                                before
                                (build-no-keep end-call after))))))))
      (list)))))



(defun split-positional/ (positional list)
  "Splits on positional - positional can be an integer zero-index, a function
  (that validates that an index is a split-point), or a list of indices that is assumed to already be sorted."
  (typecase positional
    (integer (multiple-value-bind (before after call)
                 (split-on-test/ (lambda (elt) (= (cdr elt) positional)) (map/ (lambda (elt pos) (cons elt pos)) list (iterate/ #'1+ 0)))
               (multiple-value-bind (split-val split-val-valid)
                   (funcall call)
                 (values (map/ #'car before) (map/ #'car (append/ (when split-val-valid split-val) after))))))
    (t
     (let ((split-masks
            (typecase positional
              (function (map/ positional (iterate/ #'1+ 0)))
              (t
               (map/
                (lambda (elt) (eql (car elt) (first/ (cdr elt))))
                (iterate/
                 (lambda (elt)
                   (eager
                     (destructuring-bind (index . remainder) elt (cons (1+ index) (if (eql index (car/ remainder)) (cdr/ remainder) remainder)))))
                 (cons 0 positional)))))))
       (labels ((build (split-remainder)
                  (lambda ()
                    (multiple-value-bind (head tail valid)
                        (head-tail/ split-remainder)
                        (if valid
                            (if (consp head)
                                (if (null/ tail)
                                    (values (list/ head) (lambda () nil nil))
                                  (multiple-value-bind (th tt tv)
                                      (head-tail/ tail)
                                    (if (consp th)
                                        (values (list/ head) (build tail))
                                      (values (list*/ head th) (build tt)))))
                              (values head (build tail)))
                          (call-for-end))))))
         (map/ (curried #'map/ #'car) (lazy-list-from-call (build (split-down-on-test/ #'cdr (map/ #'cons list split-masks) :keep-split-causing-elements t :keep-empty-non-split nil)))))))))



(defun take-while/ (test list)
  "Returns a lazy-list representing elements of list while test (run against values) returns true.  Performance note:  Will tend to maintain reference to the original list,
  create a new static list (via to-list or to-array) if this is a concern."
  (assert (functionp test))
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call current-call)
           (if current-call
               (with-traversal-result
                (value next)
                (get-traversal-result current-call)
                (let ((next (resolved next)))
                  (if next
                      (let ((value (resolved value)))
                        (if (funcall test value)
                            (traversal-result value (build end-call next))
                          (unresolved (get-traversal-result end-call))))
                    (unresolved (get-traversal-result end-call)))))
             (unresolved (get-traversal-result end-call))))
    ((resolved (get-call-for-first (to-lazy-list list)))))))



(defun drop-while/ (test list)
  "Returns the subset of list after test returns false - in eager context, traverses immediately - in lazy, upon first traversal of resultant lazy-list.  Performance note:  If list uses
  some form of memoization/caching, and another instance has \"cached ahead\", the result lazy-list will be bound to the cache until it can overtake it."
  (assert (functionp test))
  (labels ((get-read-point-after-test ()
             (let ((current (read-point-built (to-lazy-list list))))
               (tagbody
                top
                (when (and (not (read-point-at-end current)) (funcall test (read-point-value current)))
                  (setq current (read-point-advanced current))
                  (go top)))
               current)))
    (if-lazy-eager
     (lazy-list-from-call (fixed-traversal-link-from-result-form (get-traversal-result (call-to-detach-from-read-point (get-read-point-after-test)))))
     (lazy-list-from-call (call-to-detach-from-read-point (get-read-point-after-test))))))


(defun position/ (item list &key (test #'eql))
"Returns the first 0-index of item in list that satisfies test, nil if not found"
  (let ((current (read-point-built (to-lazy-list list)))
        (pos 0))
    (tagbody
     top
     (when (and (not (read-point-at-end current)) (not (funcall test (read-point-value current) item)))
       (setq current (read-point-advanced current))
       (incf pos)
       (go top)))
    (if (read-point-at-end current)
        nil
      pos)))



(defun nth/ (index list)
  "Equivalent of CL's NTH; but traverses CL sequences or lazy-lists"
  (assert (integerp index))
  (typecase list
    (array (aref list index))
    (list (nth index list))
    (t
     (let ((read-point (read-point-built (to-lazy-list list)))
           (known-at-end nil))
       (setq list nil) ; to help make "list" eligible for gc.
       (loop for i from 1 to index
             while (not (setq known-at-end (read-point-at-end read-point)))
             do
             (setq read-point (read-point-advanced read-point)))
       (unless known-at-end
         (read-point-value read-point))))))

(defun second/ (list)
  "Like CL's second; but can acommodate lazy-lists or CL sequences"
  (nth/ 1 list))
(defun third/ (list)
  "Like CL's third; but can acommodate lazy-lists or CL sequences"
  (nth/ 2 list))



(defun tails/ (list)
  "Returns list of lists, with each list being the (cdr/) of the previous one.  Final list in sequence is empty list."
  (labels ((tails-list-for-proper-list (list)
             (make-instance-2 'lazy-list-with-persistence
                              :call-for-first
                              (standard-traversal-link
                               (build (end-call remainder)
                                      (if remainder
                                          (traversal-result (to-lazy-list remainder) (build end-call (cdr remainder)))
                                        (traversal-result (list/) end-call)))
                               (list)))))
    (typecase list
      (list (tails-list-for-proper-list list))
      (lazy-list-list-based (tails-list-for-proper-list (get-list-head list)))
      (t
       (let ((new-class-type
              (cond
               ((typep list 'lazy-list-with-persistence) 'lazy-list-with-persistence) ; list based or with full persist, this persist (minor calc)

               ((typep list 'lazy-list-with-some-persistence) 'lazy-list-with-some-persistence) ; will not grant the memoize - may want memoized to
                                                                                                    ; surrender the readpoint
               (t 'lazy-list))))
         (make-instance-2 new-class-type
                          :call-for-first
                          (standard-traversal-link
                           (build (end-call call)
                                  (with-traversal-result (value next)
                                      (get-traversal-result call)
                                    (let ((next (resolved next)))
                                      (if next
                                          (let ((value (resolved value)))
                                            (traversal-result
                                             (make-instance-2 new-class-type :call-for-first (fixed-traversal-link value next))
                                             (build end-call next)))
                                        (traversal-result (list/) (fixed-traversal-link-from-result-form (get-traversal-result end-call)))))))
                           ((get-call-for-first (to-lazy-list list))))))))))



                                                            

(defun map/ (function first &rest other-lazy-lists)
  (assert (functionp function))
  (lazy-list-from-call
   (standard-traversal-link (build (end-call callers-list)
                                   (let ((result-stash
                                          (loop for caller in callers-list collect
                                                (with-traversal-result (value next)
                                                                       (get-traversal-result caller)
                                                                       (let ((next (resolved next)))
                                                                         (if next
                                                                             (cons next value)
                                                                           (return nil)))))))
                                     (if result-stash
                                         (traversal-result
                                          (apply function (mapcar (lambda (elt) (resolved (cdr elt))) result-stash))
                                          (build end-call (mapcar #'car result-stash)))
                                       (unresolved (get-traversal-result end-call)))))
                            ((mapcar
                              (lambda (elt) (get-call-for-first (to-lazy-list elt)))
                              (cons first other-lazy-lists))))))


(defun concat/ (list)
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call top-level-call)
           (with-traversal-result (sub-list call-for-next-sublist)
                                  (resolved (get-traversal-result top-level-call))
                                  (let ((call-for-next-sublist (resolved call-for-next-sublist)))
                                    (if call-for-next-sublist
                                        (unresolved
                                         (get-traversal-result-new-end-call
                                          (get-call-for-first (to-lazy-list (resolved sub-list)))
                                          (unresolved (build end-call call-for-next-sublist))))
                                      (unresolved (get-traversal-result end-call))))))
    ((get-call-for-first (to-lazy-list list))))))


(defun append/ (&rest list-of-lists)
  (if (cdr list-of-lists) ; i.e. more than one
      (concat/ list-of-lists)
    (to-lazy-list (first list-of-lists))))

; (A B C) == (cons A (cons B C))
(defun list*/ (&rest list-list-terminated)
  "Basically CL's list* - \"conses\" all elements but last onto list in last parameter, returning a lazy-list."
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call remainder)
           (destructuring-bind (car-remainder &rest cdr-remainder) ; so as to only capture required pieces.
               remainder
           (if cdr-remainder
               (traversal-result car-remainder (build end-call cdr-remainder))
             (unresolved (get-traversal-result-new-end-call (get-call-for-first (to-lazy-list car-remainder)) end-call)))))
    (list-list-terminated))))




(defun assoc/ (item alist &rest rest)
  "CL's assoc; but works with lazy-lists of cons pairs"
  (typecase alist
    (list (apply #'assoc item alist rest))
    (t
     (destructuring-bind (&key (test #'eql) (key #'identity))
         rest
       (let ((current (read-point-built alist)))
         (tagbody
          top
          (when (not (read-point-at-end current))
            (when (not (funcall test item (funcall key (first (read-point-value current)))))
              (setq current (read-point-advanced current))
              (go top))))
         (when (not (read-point-at-end current))
           (read-point-value current)))))))


(defun prepend/ (&rest list-list-terminated)
  "CL's list*, returning a lazy-list"
  (apply #'list*/ list-list-terminated))



(defun filter/ (predicate list)
  (lazy-list-from-call
   (standard-traversal-link
    (filter-call (end-call current-call)
                 (with-traversal-result
                  (value next)
                  (get-traversal-result current-call)
                  (let ((next (resolved next))
                        (value value))
                    (block nil
                      (tagbody
                       top
                       (if next
                           (let ((val (resolved value)))
                             (if (funcall predicate val)
                                 (return (traversal-result val (filter-call end-call next)))
                               (with-traversal-result
                                   (v2 n2)
                                   (get-traversal-result next)
                                 (setq next n2)
                                 (setq value v2)
                                 (go top))))
                         (return (get-traversal-result end-call))))))))
    ((get-call-for-first (to-lazy-list list))))))







; Needs optimization, to put 
(defun nub-by/ (equality list)
  (let ((hash (make-hash-table :test equality))
        (readpoint-seeking-end nil))
    (values
     (let ((return-list
            (memoized/
             (lazy-list-from-call
              (standard-traversal-link
                (read-point-to-call (end-call start)
                             (let ((current start)
                                   (value nil))
                               (tagbody
                                top
                                (when (not (read-point-at-end current))
                                  (setq value (read-point-value current))
                                  (multiple-value-bind (current-val current-val-valid)
                                      (gethash value hash)
                                    (if current-val-valid
                                        (progn
                                          (setq current (read-point-advanced current))
                                          (go top))
                                      (setf (gethash value hash) t)))))
                               (if (read-point-at-end current)
                                   (unresolved (get-traversal-result end-call))
                                 (traversal-result
                                  value
                                  (read-point-to-call end-call (read-point-advanced current))))))
                ((read-point-built (to-lazy-list list))))))))
       (setq readpoint-seeking-end (read-point-built return-list))
       return-list)
     (lambda (key)
       (when readpoint-seeking-end
         (loop while (not (read-point-at-end readpoint-seeking-end)) do (setq readpoint-seeking-end (read-point-advanced readpoint-seeking-end))))
       (multiple-value-bind (dummy exists) (gethash key hash) exists)))))

(defun nub/ (list) (nub-by/ #'eql list))

(defun and/ (list)
  "Returns last element or nil"
  (let ((last nil))
    (block :exit
      (loop-over/ elt (to-lazy-list list)
        (if elt
            (setq last elt)
          (return-from :exit nil)))
      last)))

(defun or/ (list)
;  "Returns first non-nil element or nil"
  (block :exit
    (loop-over/ elt (to-lazy-list list)
      (if elt
          (return-from :exit elt)))
    nil))

(defun latch-on/ (func list &key (initial-value nil))
  (labels ((build (read-point latched latched-val)
             (lambda ()
               (if (read-point-at-end read-point)
                   (call-for-end)
                 (if latched
                     (values latched-val (build (read-point-advanced read-point) t latched-val))
                   (let ((val (read-point-value read-point)))
                     (if (funcall func val)
                         (values val (build (read-point-advanced read-point) t val))
                       (values
                        initial-value
                        (build (read-point-advanced read-point) nil nil)))))))))
    (lazy-list-from-call (build (read-point-built (to-lazy-list list)) nil nil))))

(defun foldl/ (function first list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end nil :initial-value first))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end nil :initial-value first))
    (lazy-list
     (let ((accum first)
           (current (get-call-for-first list)))
       (tagbody
        top
        (with-traversal-result (val next)
            (get-traversal-result current)
            (let ((next (resolved next)))
              (when next
                (setf accum (funcall function accum (resolved val)))
                (setf current next)
                (go top)))))
       accum))))




(defun foldl1/ (function list)
  (typecase list
    (sequence (reduce function list :from-end nil))
    (lazy-list-list-based (reduce function (get-list-head list) :from-end nil))
    (t
     (with-traversal-result (value next)
         (get-traversal-result (get-call-for-first (to-lazy-list list)))
         (let ((current (resolved next))
               (accum (resolved value)))
           (loop while current do
                 (with-traversal-result (value next)
                                        (get-traversal-result current)
                                        (let ((next (resolved next)))
                                          (setq current next)
                                          (when next
                                            (setq accum (funcall function accum (resolved value)))))))
           accum)))))



(defun foldr/ (function first list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end t :initial-value first))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end t :initial-value first))
    (lazy-list
     (reduce function (to-list list) :from-end t :initial-value first))))

(defun foldr1/ (function list)
  (assert (functionp function))
  (etypecase list
    (sequence (reduce function list :from-end t ))
    (lazy-list-list-based
     (reduce function (get-list-head list) :from-end t))
    (lazy-list
     (reduce function (to-list list) :from-end t ))))


(defun scanl/ (function first list)
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call first call)
           (with-traversal-result
            (val next)
            (get-traversal-result call)
            (let ((next (resolved next)))
              (if next
                  (traversal-result first (build end-call (funcall function first (resolved val)) next))
                (traversal-result first end-call)))))
    (first (get-call-for-first (to-lazy-list list))))))


(defun scanl1/ (function list)
  (lazy-list-from-call
   (standard-traversal-link
    (build (end-call first first-valid call)
           (with-traversal-result
            (val next)
            (get-traversal-result call)
            (let ((next (resolved next)))
              (if next
                  (if first-valid
                      (traversal-result first (build end-call (funcall function first (resolved val)) t next))
                    (unresolved (get-traversal-result (build end-call (resolved val) t next))))
                (traversal-result first end-call)))))
    (nil nil (get-call-for-first (to-lazy-list list))))))


(defun scanr/ (function first list)
  (labels ((build-result-list-call ()
             (let ((result (list first)))
               (loop for elt in (reverse (to-list list)) do
                     (push (funcall function elt (car result)) result))
               (list-to-lazy-list-call result))))
    (if-lazy-eager
     (lazy-list-from-call
      (deferred-traversal-link-from-call-maker (build-result-list-call)))
     (lazy-list-from-call (build-result-list-call)))))



(defun scanr1/ (function list)
  (labels ((build-result-list-call ()
             (let* ((reversed (reverse (to-list list)))
                    (result (list (first reversed))))
               (if reversed
                   (progn
                     (loop for elt in (cdr reversed) do
                           (push (funcall function elt (car result)) result))
                     (list-to-lazy-list-call result))
                 (list-to-lazy-list-call nil)))))
    (if-lazy-eager
     (lazy-list-from-call
      (deferred-traversal-link-from-call-maker (build-result-list-call)))
     (lazy-list-from-call (build-result-list-call)))))



(defun grouped-by-firsts/ (test list-of-pair-conses)
  (labels ((build-list ()
             (let ((hash (make-hash-table :test test)))
               (loop-over/
                   elt
                   (to-lazy-list list-of-pair-conses)
                 (destructuring-bind (key . value) (to-list elt)
                   (setf (gethash key hash) (cons value (gethash key hash)))))
               (values
                (get-call-for-first
                 (map/ (lambda (key) (cons key (gethash key hash)))
                       (loop for key being the hash-keys of hash collect (progn (setf (gethash key hash) (nreverse (gethash key hash))) key))))
                (lambda (key)
                  (gethash key hash))))))
    (if-lazy-eager
     (let ((build-call-values-list nil))
       (labels ((verify-data-ready ()
                  (cond (build-call-values-list)
                        (t (setq build-call-values-list (multiple-value-list (build-list)))))))
         (values
          (lazy-list-from-call
           (fixed-traversal-link-from-result-form (get-traversal-result (first (verify-data-ready)))))
          (second (verify-data-ready)))))
     (multiple-value-bind (list query)
         (build-list)
       (values
        (lazy-list-from-call list)
        query)))))



(defun grouped-cdrs-by-car/ (list-of-cons-pairs &key (test 'eql))
  "Takes a list of cons pairs, of the form (first . second) - returns a list of conses of the form (first . (second 0 second1 second2 second3....)) as the first value, where the seconds are matches on first.
Second return value is a function, that returns a list of seconds based on a search key/first as first value, found (T or NIL) as second.
When run in an eager context, grouped-by-first-in-cons-pairs/ calculates the internal hash immediately.
When run in a lazy context, the creation of the internal hash is deferred - and is on the first request of either the resultant list, or execution of the second return value."
  (grouped-by-firsts/ test list-of-cons-pairs))

(defun grouped-seconds-by-first/ (list-of-list-pairs &key (test 'eql))
  "Equivalent to grouped-cdrs-by-car/ , except that the input pairs come as a list of lists, instead of a list of conses."
  (grouped-by-firsts/ test (map/ (curried #'apply #'cons) list-of-list-pairs)))




(defun sort-by/ (ordering list)
  (assert (functionp ordering))
  (if-lazy-eager
   (lazy-list-from-call
    (fixed-traversal-link-from-result-form
     (let ((sorted (to-list list)))
       (sort sorted ordering)
       (get-traversal-result
        (get-call-for-first (to-lazy-list sorted))))))
   (let ((sorted (to-list list)))
     (sort sorted ordering)
     (to-lazy-list sorted))))

(defun sort/ (list)
  (sort-by/ #'< list))






(defmacro self-ref-list/ (ref-name &body definition)
  (let ((self-sym (gensym))
        (end-call-sym (gensym)))
    `(lazy-list-from-call
      (fixed-traversal-link-from-result-form
        (get-traversal-result
         (get-call-for-first
          (let ((,self-sym :error))
            (symbol-macrolet ((,ref-name (lazy-list-from-call (fixed-traversal-link-from-result-form (get-traversal-result ,self-sym)))))
              (let* ((ref (memoized/ (lazy ,@definition)))
                     (call (get-call-for-first ref)))
                (setq ,self-sym call)
                ref)))))))))



(defmacro let/ (definitions &body body)
  `(let
       ,(mapcar
         (lambda (entry)
           (print entry)
           (if (consp entry)
               (destructuring-bind (var-name value-clause)
                   (if (consp entry) entry (list entry nil))
                 `(,var-name
                   (self-ref-list/ ,var-name ,value-clause)))
             entry))
         definitions)
     ,@body))



(defparameter **unit-tests**
  `(
    ("To-from-list"
     (
      ("Proper List" (let ((lista (loop for elt from 1 to 10 collect elt))
                           (listb (loop for elt from 1 to 10 collect elt)))
                       (values (and (equal (to-list (to-lazy-list lista)) listb) (equal lista listb)))))

      ("Array" (let ((arraya (make-array 10 :initial-contents (loop for elt from 1 to 10 collect elt)))
(arrayb (make-array 10 :initial-contents (loop for elt from 1 to 10 collect elt))))
                 (values (and (equal (to-list (to-lazy-list arraya)) (map 'list #'identity arrayb)) (equalp arraya arrayb)))))))

    ("Standard operations"
     ,(apply #'nconc
             (loop for in-type in `(("list" ,#'identity)
                                    ("list-based lazy list" ,#'to-lazy-list)
                                    ("lazy-list eager-value" ,(curried #'map/ #'identity))
                                    ("lazy-list lazy-value" ,(curried #'map/ (lambda (elt) (unresolved elt))))) collect
                   (mapcar
                    (lambda (test)
                      `(,(concatenate 'string (first test) " " (first in-type))
                        ,(second test)))
                    (labels ((transformed (list) `(funcall ,(second in-type) ',list)))
                      (let ((equality-check-sets
                             `(("foldl/" (= (foldl/ #'/ 64 ,(transformed '(4 2 4))) 2))
                               ("foldl1/" (= (foldl1/ #'/ ,(transformed '(64 4 2 8))) 1))
                               ("foldr/" (= (foldr/ #'/ 2 ,(transformed '(8 12 24 4))) 8))
                               ("foldr1/" (= (foldr1/ #'/ ,(transformed '(8 12 24 4))) 4))

                               ("head-tail/ multiple"
                                (equal
                                 (multiple-value-bind (head tail) (head-tail/ ,(transformed '(1 2 3 4))) (list head (to-list tail)))
                                 '(1 (2 3 4))))
                               ("head-tail/ short"
                                (equal
                                 (multiple-value-bind (head tail) (head-tail/ ,(transformed '(1))) (list head (to-list tail)))
                                 '(1 nil)))
                               ("nth/ empty"
                                (equal (nth/ 3 ,(transformed nil)) nil))
                               ("nth/ stocked"
                                (equal (nth/ 3 ,(transformed '(10 20 30 40))) 40))

                               ("assoc/"
                                (equal (assoc/ 3 ,(transformed '((2 . "A") (3 . "B") (4 . "C")))) '(3 . "B")))

                               ("and/ true" (and/ ,(transformed '(1 2 3 4))))
                               ("and/ false" (equal (and/ ,(transformed '(1 2 nil 3 4))) nil))

                               ; ("scanl/" (equal (to-list (scanl/ #'/ 64 ,(transformed '(4 2 4)))) '(64 16 8 2)))
                               ; ("scanl1/" (equal (to-list (scanl1/ #'/ ,(transformed '(64 4 2 8)))) '(64 16 8 1)))
                               ; ("scanr/" (equal (to-list (scanr/ #'/ 2 ,(transformed '(8 12 24 4)))) '(8 1 12 2 2)))
                               ; ("scanr1/" (equal (to-list (scanr1/ #'/ ,(transformed '(8 12 24 2)))) '(8 1 12 2)))
                               ; ("nub/" (equal (to-list (nub/ ,(transformed '(9 8 4 4 1 4 9)))) '(9 8 4 1)))
                               ; ("append/" (equal (to-list (append/ ,(transformed '(1 2 3 4)) ,(transformed '(5 6 7 8)))) '(1 2 3 4 5 6 7 8)))
                               ; ("concat/" (equal (to-list (concat/ ,(transformed '((1 2 3) (4 5 6))))) '(1 2 3 4 5 6)))
                               ("car/" (equal (car/ ,(transformed '(5 6 7 8))) 5))
                               ; ("cdr/" (equal (to-list (cdr/ ,(transformed '(5 6 7 8)))) '(6 7 8)))
                               ; ,@(loop for i from 0 to 10 collect `(,(format nil "nthcdr/ ~S" i) (equal (to-list (nthcdr/ ,i ,(transformed (loop for i from 1 to 8 collect i)))) (nthcdr i (loop for i from 1 to 8 collect i)))))
                               ))
                            (list-checks
                             `(("scanl/" (scanl/ #'/ 64 ,(transformed '(4 2 4))) '(64 16 8 2))
                               ("scanl1/" (scanl1/ #'/ ,(transformed '(64 4 2 8))) '(64 16 8 1))
                               ("scanr/" (scanr/ #'/ 2 ,(transformed '(8 12 24 4))) '(8 1 12 2 2))
                               ("scanr1/" (scanr1/ #'/ ,(transformed '(8 12 24 2))) '(8 1 12 2))
                               ("nub/" (nub/ ,(transformed '(9 8 4 4 1 4 9))) '(9 8 4 1))
                               ("append/" (append/ ,(transformed '(1 2 3 4)) ,(transformed '(5 6 7 8))) '(1 2 3 4 5 6 7 8))
                               ("concat/" (concat/ ,(transformed '((1 2 3) (4 5 6)))) '(1 2 3 4 5 6))
                               ("intersperse/ empty-case" (intersperse/ "Intersp" ,(transformed nil)) nil)
                               ("intersperse/ single-case" (intersperse/ "Intersp" ,(transformed '(1))) '(1))
                               ("intersperse/ multiple-case" (intersperse/ "Intersp" ,(transformed '(1 2 3 4))) '(1 "Intersp" 2 "Intersp" 3 "Intersp" 4))
                               ("take-while/ empty-case" (take-while/ #'identity ,(transformed nil)) nil)
                               ("take-while/ single-case inc" (take-while/ #'identity ,(transformed '(1))) '(1))
                               ("take-while/ single-case exc" (take-while/ #'null ,(transformed '(1))) nil)
                               ("take-while/ multiple partial" (take-while/ #'identity ,(transformed '(1 2 nil 3 4))) '(1 2))
                               ("take-while/ multiple end" (take-while/ #'identity ,(transformed '(1 2 3 4 nil))) '(1 2 3 4))
                               ("take-while/ multiple start" (take-while/ #'identity ,(transformed '(nil 1 2 3 4))) nil)
                               ("take-while/ multiple full" (take-while/ #'identity ,(transformed '(1 2 3 4))) '(1 2 3 4))

                               ("drop-while/ empty-case" (drop-while/ #'identity ,(transformed nil)) nil)
                               ("drop-while/ single-case inc" (drop-while/ #'identity ,(transformed '(1))) nil)
                               ("drop-while/ single-case exc" (drop-while/ #'null ,(transformed '(1))) '(1))
                               ("drop-while/ multiple partial" (drop-while/ #'identity ,(transformed '(1 2 nil 3 4))) '(nil 3 4))
                               ("drop-while/ multiple end" (drop-while/ #'identity ,(transformed '(1 2 3 4 nil))) '(nil))
                               ("drop-while/ multiple start" (drop-while/ #'identity ,(transformed '(nil 1 2 3 4))) '(nil 1 2 3 4))
                               ("drop-while/ multiple full" (drop-while/ #'identity ,(transformed '(1 2 3 4))) nil)

                               ("list*/ empty" (list*/ ,(transformed nil)) nil)
                               ("list*/ frontloaded only" (list*/ 1 2 3 4 ,(transformed nil)) '(1 2 3 4))
                               ("list*/ backloaded only" (list*/ ,(transformed '(1 2 3 4))) '(1 2 3 4))
                               ("list*/ mixed" (list*/ 1 2 ,(transformed '(3 4))) '(1 2 3 4))

                               ("tails/" (map/ #'to-list (tails/ ,(transformed '(1 2 3 4)))) '((1 2 3 4) (2 3 4) (3 4) (4) nil))

                               ("filter/" (filter/ #'identity ,(transformed '(1 2 nil 3 4))) '(1 2 3 4))

                               ("grouped-by-firsts/" (grouped-by-firsts/ #'eql '((1 . 2) (1 . 3) (2 . 4) (1 . 2))) '((1 2 3 2) (2 4)))
                               ("sort/" (sort/ ,(transformed '(5 3 2 1 9))) '(1 2 3 5 9))
                               ("iterate/" (take/ 100 (iterate/ #'1+ 1)) (loop for i from 1 to 100 collect i))

                               ("cdr/" (cdr/ ,(transformed '(5 6 7 8))) '(6 7 8))

                               ,@(loop for i from 0 to 10 collect
                                       `(,(format nil "nthcdr/ ~S" i) (nthcdr/ ,i ,(transformed (loop for i from 1 to 8 collect i))) (nthcdr ,i (loop for i from 1 to 8 collect i))))
                               ,@(loop for i from 0 to 10 collect
                                       `(,(format nil "take/ ~S" i) (take/ ,i ,(transformed (loop for i from 1 to 8 collect i))) (subseq (loop for i from 1 to 8 collect i) 0 (min ,i 8))))

                               )))
                        (append
                         equality-check-sets
                         (mapcan (lambda (elt)
                                   (destructuring-bind (name lazy-form result-form)
                                       elt
                                     `((,name (equal (to-list ,lazy-form) ,result-form))
                                       (,(concatenate 'string name " concat test")
                                        (equal
                                         (to-list (append/ (append/ ,lazy-form ,lazy-form ,lazy-form) (append/ ,lazy-form ,lazy-form ,lazy-form)))
                                         (append (append ,result-form ,result-form ,result-form) (append ,result-form ,result-form ,result-form)))))))
                                 list-checks))))))))
    
    ("Multifunctionality Sanity tests"
     (

      ("Cliched Fibonacci to 100"
       (funcall (compile nil (lambda () (= (nth/ 100 (self-ref-list/ fib (list*/ 1 1 (map/ #'+ fib (tail/ fib))))) 573147844013817084101)))))

#|
      ("Fibonacci capped but taken to 1,000,000 - leak test"
       (nth/ 1000000
             (self-ref-list/ fib (list*/ 1 1 (map/ (curried #'max 10) fib (tail/ fib))))))
|#
      ("CSV Parsing Test (split-down-on-test and others)"
       (let ((csv-file

              "1,2, 3 , I contain \" Quoted, commas, \" you see, 99
g, \"hijk\"lmn
third_line,stuff here"))
         (equal
          '(("1" "2" " 3 " " I contain \" Quoted, commas, \" you see" " 99") ("g" " \"hijk\"lmn") ("third_line" "stuff here"))
          (to-list
           (map/ (composed #'to-list (curried #'map/ (composed #'to-string (curried #'map/ #'car))))
                 (map/ (composed
                        (lambda (line) (split-down-on-test/ (curried #'equal '(#\, . nil)) line))
                        (curried #'scanl1/ (lambda (a b) (cons (car b) (if (cdr a) (not (cdr b)) (cdr b)))))
                        (curried #'map/ (lambda (elt) (cons elt (eql elt #\")))))
                       (split-down-on-test/ (curried #'eql #\newline) csv-file)))))))

))



    #|
    ("grouped-by-firsts*"
     ,(let ((pairs-as-lists '(("Brown" "Bill") ("Smith" "Ian") ("Stein" "Fred") ("Brown" "Sarah") ("Brown" "Lance"))))
        (eager
          (multiple-value-bind (result-pairs query)
              (grouped-seconds-by-first/ pairs-as-lists :test 'equal)
            ))
        )
     )
|#
    ))

(defun unit-test ()
  (loop for named-test in **unit-tests** do
        (destructuring-bind (identity sub-test-list)
            named-test
          (format t "Testing ~A" (string identity))
          (terpri)
          (force-output)
          (loop for laziness in '(eager lazy) do
                (loop for sub-test in sub-test-list for sub-test-number from 1 do
                      (destructuring-bind (sub-test-identity test)
                          (if (> (length sub-test) 1)
                              sub-test
                            (list (format nil "~A: " sub-test-number) (car sub-test)))
;                  (format t "    ~A: " sub-test-identity)
;                  (force-output)
                        (let ((identity-string (format nil "    ~A in ~A mode: " sub-test-identity laziness)))
                          (handler-case
                              (multiple-value-bind (passed info)
                                  (eval `(,laziness ,test))
                                (if passed
                                    nil ;(format t (concatenate 'string identity-string " passed"))
                                  (progn (format t (concatenate 'string identity-string (format nil "!!!failed!!!") (when info (format nil " ~A" info)))) (terpri)))
                                (force-output))
                            (error (err)
                              (format t (concatenate 'string identity-string "failed hard ~A ~S") err err)
                              (terpri)
                              (force-output)))))
                      (force-output)))
          (terpri)
          (force-output))))

(unit-test)



