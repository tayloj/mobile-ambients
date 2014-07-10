;;==================================================================;;
;; Joshua Taylor
;; December 9, 2009
;; tayloj@cs.rpi.edu
;;==================================================================;;

(defpackage #:mobile-ambients-examples
  (:documentation "The symbols exported from this package are not
likely to be of too much use on their own, but rather
``examples.lisp'' should be read in parallel with Cardelli & Gordon's
``Mobile ambients.'' The section number of this file follows their
paper, and most of the examples have worked out here.")
  (:use #:mobile-ambients "COMMON-LISP")
  (:nicknames #:amb-ex)
  (:shadowing-import-from #:mobile-ambients
   #:open)
  (:shadow #:get #:set)
  (:export ; A Generic Process engine
   #:shorten
   #:generic-process-example)
  (:export ; 2.4.1 Locks
   #:acquire
   #:release
   #:lock-example)
  (:export ; 2.4.2 Mobile agent authentication
   #:authentication-example)
  (:export ; 2.4.3 Firewall access
   #:firewall
   #:agent
   #:firewall-example)
  (:export ; 2.4.4 Movement ... subjective vs. objective
   #:obj-in #:obj-in^
   #:obj-out #:obj-out^
   #:obj-in-example
   #:obj-out-example
   #:entrap
   #:entrap-example)
  (:export ; 2.4.5 Dissolution
   #:acid #:acid^
   #:acid-example
   #:acid-in
   #:acid-out
   #:acid-in-example
   #:acid-out-example
   #:acid*
   #:acid*-example)
  (:export ; 2.4.6 Encoding objective moves
   #:allow
   #:mv-in
   #:mv-out
   #:ambi
   #:ambo
   #:ambio
   #:objective-move-example-1
   #:objective-move-example-2)
  (:export ; 2.4.7 Synchronization on named channels
   #:?
   #:!
   #:synchronization-example)
  (:export ; 2.4.8 Choice
   #:choice
   #:choice-example)
  (:export ; 2.4.9 Renaming
   #:be
   #:be-example)
  (:export ; 2.4.10 Seeing
   #:see
   #:see-example)
  (:export ; 2.4.11 Iteration
   #:rec-fn
   #:rec
   #:rec-example)
  (:export ; 2.4.12 Numerals
   #:|0|
   #:|i+1|
   #:ifzero
   #:numeral-example-1
   #:numeral-example-2
   #:inc
   #:inczero
   #:incsucc
   #:dec
   #:numeral-example-3
   #:numeral-example-3)
  (:export ; 3.4.1 Cells
   #:cell
   #:get-for
   #:get
   #:set
   #:get-and-set-in
   #:get-and-set
   #:cell-example-1
   #:cell-example-2)
  (:export ; Safe Ambients
   #:$in #:$in^ #:$out #:$out^ #:$open #:$open^
   #:in$ #:in$^ #:out$ #:out$^ #:open$ #:open$^
   #:safe-ambients-example-1
   #:safe-ambients-example-2
   #:safe-ambients-example-3)
  (:export ; Miscellaneous Examples
   #:tree-reduce
   #:tree-reduce-example-1
   #:tree-reduce-example-2))

(in-package #:mobile-ambients-examples)

(defmacro defexample (name &body body)
  (let ((args (gensym (string '#:args-))))
    `(defun ,name (&rest ,args)
       (apply 'run-process
              (progn (par ,@body))
              ,args))))

;;; A Generic Process Engine

(defun shorten (list)
  (setf (rest list) (rest (rest list))))

(defmethod evolutions ((list list))
  (if (endp (rest list)) '()
    (list #'(lambda () (shorten list)))))

(defmethod cleanup ((list list))
  (declare (ignore list))
  nil)

(defun generic-process-example ()
  (run-process
   (list 1 2 3 4 5)))

;;; Mobile Ambients

;; 2.4.1 Locks

(defun acquire (n P)
  (open n P))

#+lispworks (editor:setup-indent "acquire" 1)

(defun release (n P)
  (par (amb n) P))

(defexample lock-example 
  (acquire 'n (release 'm (named 'P)))
  (release 'n (acquire 'm (named 'Q))))

;; 2.4.2 Mobile agent authentication

(defexample authentication-example
  (amb 'home
    (new (n)
      (par (open n)
           (amb 'agent
             (out 'home
                  (in 'home
                      (amb n
                        (out 'agent
                             (open 'agent (named 'p)))))))))))

;; 2.4.3 Firewall access

(defun firewall ()
  (new (w)
       (amb w
            (amb 'k (out w (in 'k2 (in w))))
            (open 'k2 (open 'k3 (named 'p))))))

(defun agent ()
  (amb 'k2
       (open 'k
             (amb 'k3
                  (named 'Q)))))

(defexample firewall-example
  (firewall)
  (agent))

;; 2.4.4 Movement ... subjective vs. objective

(defclass obj-in (capability) ()
  (:documentation "Objective entry capability."))

(defun obj-in^ (name)
  (make-instance 'obj-in :name name))

(defun obj-in (name process)
  (cap (obj-in^ name) process))

(defmethod write-process (stream (oi obj-in))
  (format stream "omv in ~:w" (capability-name oi)))

;; this implementation uses less LOOP
#-(and)
(defmethod action-evolutions ((oi obj-in) (action action))
  (let ((parent (process-parent action))
        (aprocess (action-process action))
        (name (capability-name oi))
        (evolutions '()))
    (dolist (sibling (process-siblings action))
      (when (and (typep sibling 'ambient)
                 (eql name (ambient-name sibling)))
        (let ((sibling sibling))
          (push #'(lambda ()
                    (setf (process-parent aprocess) sibling)
                    (push aprocess (composition-processes sibling))
                    (setf (composition-processes parent)
                          (delete action
                                  (composition-processes parent))))))))))

;; this implementation uses more LOOP
(defmethod action-evolutions ((oi obj-in) (action action))
  (loop with parent = (process-parent action)
        with aprocess = (action-process action)
        with name = (capability-name oi)
        for sibling in (process-siblings action)
        when (amb::ambientp sibling name)
        collect (let ((target sibling))
                  #'(lambda ()
                      (setf (process-parent aprocess) target)
                      (push aprocess (composition-processes target))
                      (setf (composition-processes parent)
                            (delete action
                                    (composition-processes parent)))))))

(defexample obj-in-example
  (obj-in 'm (named 'P))
  (amb 'm (named 'R)))

(defclass obj-out (capability) ()
  (:documentation "Objective exit capability."))

(defun obj-out^ (name)
  (make-instance 'obj-out :name name))

(defun obj-out (name process)
  (cap (obj-out^ name) process))

(defmethod write-process (stream (oo obj-out))
  (format stream "omv out ~:w" (capability-name oo)))

(defmethod action-evolutions ((oo obj-out) (action action))
  (let ((parent (process-parent action))
        (aprocess (action-process action)))
    (if (not (amb::ambientp parent (capability-name oo))) '()
      (let ((pparent (process-parent parent)))
        (list #'(lambda ()
                  (setf (process-parent aprocess) pparent)
                  (setf (composition-processes parent)
                        (delete action (composition-processes parent)))
                  (push aprocess (composition-processes pparent))))))))

(defexample obj-out-example
  (amb 'm
    (obj-out 'm (named 'P))
    (named 'R)))

(defun entrap (m)
  (new (k)
    (par (amb k)
         (obj-in m (in k)))))

(defexample entrap-example
  (entrap 'm)
  (amb 'm (named 'P)))

;; 2.4.5 Dissolution

(defclass acid (capability) ()
  (:documentation "A dissolution capability."))

(defun acid^ ()
  (make-instance 'acid))

(defmethod write-process (stream (acid acid))
  (declare (ignore acid))
  (write-string "acid" stream))

(defun acid (process)
  (cap (acid^) process))

(defmethod action-evolutions ((acid acid) (action action))
  (let ((parent (process-parent action)))
    (if (not (amb::ambientp parent)) '()
      (list #'(lambda () (amb::open-ambient action parent))))))

(defexample acid-example
  (amb 'm
    (acid (named 'P))
    (named 'Q)))

(defun acid-in (n P)
  (new (q)
    (amb q (in n (acid P)))))

(defun acid-out (n P)
  (new (q)
    (amb q (out n (acid P)))))

(defexample acid-in-example
  (acid-in 'm (named 'P))
  (amb 'm (named 'R)))

(defexample acid-out-example
  (amb 'm (acid-out 'm (named 'P)) (named 'R)))

(defun acid* (n &optional (P (zero)))
  (amb 'acid (out n (open n P))))

(defexample acid*-example
  (amb 'n
    (acid* 'n (named 'P))
    (named 'Q))
  (open 'acid))

;; 2.4.6 Objective moves

(defun allow (n)
  (open n (fun (allow n))))

(defun mv-in (n P &optional (enter 'enter))
  (new (k)
    (amb k (in n (amb enter (out k (open k P)))))))

(defun mv-out (n P &optional (exit 'exit))
  (new (k)
    (amb k (out n (amb exit (out k (open k P)))))))

(defun ambi (n P &optional (enter 'enter))
  (amb n
       (par P)
       (allow enter)))

(defun ambo (n P &optional (exit 'exit))
  (par (amb n P)
       (allow exit)))

(defun ambio (n &optional (P (zero)) (enter 'enter) (exit 'exit))
  (par (amb n (par P (allow enter)))
       (allow exit)))

(defexample objective-move-example-1
  (mv-in 'n (named 'P))
  (ambio 'n (named 'Q)))

(defexample objective-move-example-2
  (ambio 'n
         (par (mv-out 'n (named 'P))
              (named 'Q))))

;; 2.4.7 Synchronization on named channels

(defun ? (n P)
  (mv-in n (acquire 'rd (release 'wr (mv-out n P)))))

(defun ! (n P)
  (mv-in n (release 'rd (acquire 'wr (mv-out n P)))))

(defexample synchronization-example
  (ambio 'a)
  (! 'a (named 'P))
  (? 'a (named 'Q)))
            
;; 2.4.8 Choice

(defun choice (n P m Q)
  (new (a b c)
    (par (amb a (in n (out n (amb b (out a (open c P))))))
         (amb a (in m (out m (amb b (out a (open c Q))))))
         (open b)
         (amb c))))

(defexample choice-example
  (choice 'n (named 'P)
          'm (named 'Q))
  (amb 'n (named 'R)))

;; 2.4.9 Renaming

(defun be (n m P)
  (par (amb m (out n (open n P))) (in m)))

(defexample be-example
  (amb 'n
    (be 'n 'm (named 'P))
    (named 'Q)))

;; 2.4.10 Seeing

(defun see (n P)
  (new (r s)
    (par (amb r (in n (out n (be r s P))))
         (open s))))

(defexample see-example
  (amb 'n)
  (see 'n (named 'P)))

;; 2.4.11 Iteration

(defun rec-fn (clauses process)
  "* Syntax:
rec-fn clauses process => rec-process
* Arguments and Values:
- clauses---a plist of ambient names and process generating functions
- process, rec-process---processes
* Description:
rec-fn is based on Cardelli & Gordon's rec, about which they say:
``The following iteration construct has a number of branches (mi)Pi
and a body Q.  Each branch can be triggered by exposing an ambient
mi[] in the body, which is then replaced by a copy of Pi.'' rec-fn
implements this construct as follows.  Clauses is a property list of
ambient names \(objects) and functions of no arguments each of which,
when called, produces a process.  Specifically, when an ambient with a
given name appears in the body, the corresponding function is called
to generate a process.  The primary purpose of rec-fn is its use in
implementing the friendlier {defmacro amb-ex::rec} .
* See Also:
- {defmacro amb-ex::rec}"
  (labels ((recopen (name alternative-gen)
             (open name
                   (par (funcall alternative-gen)
                        (fun (recopen name
                                      alternative-gen))))))
    (if (endp clauses) process
      (par (recopen (first clauses) (second clauses))
           (rec-fn (rest (rest clauses)) process)))))

(defmacro rec (clauses &body processes)
  "* Syntax:
rec ({(var form*)}) forms* => process
* Arguments and Values:
- var---a symbol naming a variable
- form---a form evaluated to produce a process
- process---a {defclass amb::process}
* Description:
The rec iteration and binding construct is described in section 2.4.11
of Cardelli & Gordon's ``Mobile ambients''.  Each var is bound to a
new ambient name and within the body of the rec \(a parallel
composition of the processes produced by the body forms), if an
ambient is created with a given name, then a parallel composition of
the processes associated with the name is generated.
* Examples:
;;; (run-process
;;;  (rec ((a (fun (print 'a1))
;;;           (fun (print 'a2)
;;;             (amb b)))
;;;        (b (fun (print 'b1))
;;;           (fun (print 'b2))))
;;;    (amb a))
;;;  nil)
;;; =>
;;; A1 
;;; A2 
;;; B2 
;;; B1 
;;; #<open #:A.({proc} | {proc} | {proc})
;;; | open #:B.({proc} | {proc} | {proc})>
* See Also:
- {defun amb-ex::rec-fn}"
  `(new ,(mapcar 'first clauses)
     (rec-fn (list ,@(mapcan
                      #'(lambda (clause)
                          (list (first clause)
                                `#'(lambda ()
                                     ,(list* 'par (rest clause)))))
                      clauses))
             (par ,@processes))))

(defexample rec-example
  (rec ((a (fun (print 'a)))
        (b (fun (print 'b)))
        (c (fun (print 'c))))
    (amb a)))

;; 2.4.12 Numerals

(defun |0| ()
  (amb 'zero))

(defun |i+1| (i)
  (amb 'succ (open 'op) i))

(defun ifzero (P Q)
  (choice 'zero P
          'succ Q))

(defexample numeral-example-1
  (|0|)
  (ifzero (named 'P)
          (named 'Q)))

(defexample numeral-example-2
  (|i+1| (|i+1| (|0|)))
  (ifzero (named 'P)
          (named 'Q)))

(defun inc (P)
  (ifzero (inczero P)
          (incsucc P)))

(defun inczero (P)
  (open 'zero
        (par (|i+1| (|0|))
             P)))

(defun incsucc (process)
  (new (p q)
    (par (amb p (amb 'succ (open 'op)))
         (open q (open p process))
         (amb 'op
              (in 'succ
                  (in p
                      (in 'succ
                          (par (amb q (out 'succ (out 'succ (out p))))
                               (open 'op)))))))))

(defun dec (process)
  (new (p)
    (par (amb 'op
              (in 'succ
                  (amb p (out 'succ))))
         (open p (open 'succ process)))))

(defexample numeral-example-3
  (|i+1| (|i+1| (|i+1| (|0|))))
  (inc (named 'P)))

(defexample numeral-example-4
  (|i+1| (|i+1| (|i+1| (|0|))))
  (dec (named 'P)))

;; 3.4.1 Cells

(defun cell (cell-name value)
  (ambio cell-name (output value)))

(defun get-for (cell process-function)
  (mv-in cell (input x
                (par (output x)
                     (mv-out cell (funcall process-function x))))))

(defmacro get (cell var &body process)
  `(get-for ,cell #'(lambda (,var) ,@process)))

(defun set (cell value &optional (process (zero)))
  (mv-in cell
         (input x
           (declare (ignore x))
           (par (output value)
                (mv-out cell process)))))

(defun get-and-set-in (cell value process-function)
  (mv-in cell (input x
                (par (output value)
                     (mv-out cell (funcall process-function x))))))

(defmacro get-and-set (cell var value &body process)
  `(get-and-set-in ,cell ,value #'(lambda (,var) ,@process)))

(defexample cell-example-1
  (cell 'c 1)
  (get-and-set 'c v1 2
    (get-and-set 'c v2 3
      (get-and-set 'c v3 4
        (get-and-set 'c v4 5
          (fun (format t "~&;; *** ~A ~A ~A ~A "
                       v1 v2 v3 v4)))))))

(defexample cell-example-2
  (cell 'c 0)
  (set 'c 1)
  (set 'c 2)
  (set 'c 3)
  (set 'c 4)
  (set 'c 5)
  (get 'c w
    (get 'c x
      (get 'c y
        (get 'c z
          (fun (format t "~&;; *** ~A ~A ~A ~A"
                       w x y z)))))))

;;; Mobile Safe Ambients

(defmacro defcapability (name)
  "Define a class named NAME that includes capability as a superclass,
a write method for the class, a constructor NAME^ and function NAME
that constructs a process with prefixed with an instance of the new
capability with the specified name."
  (let ((constructor (intern (concatenate 'string (string name) "^")))
        (stream (gensym (string '#:stream-)))
        (cname (gensym (string '#:cname-)))
        (process (gensym (string '#:process-))))
    `(progn
       (defclass ,name (capability)
         ())
       (defmethod write-process (,stream (,name ,name))
         (format ,stream "~(~:w~) ~:w"
                 ',name (capability-name ,name)))
       (defun ,constructor (,cname)
         (make-instance ',name :name ,cname))
       (defun ,name (,cname ,process)
         (cap (,constructor ,cname) ,process)))))

;; $x tries to achieve x, and x$ allows x to happen.

(defcapability $in)
(defcapability in$)
(defcapability $out)
(defcapability out$)
(defcapability $open)
(defcapability open$)

(defmethod action-evolutions (($in $in) (action action))
  ;; n[$in m.P] | m[in$ m.Q] => m[n[P] | Q]
  (if (not (amb::ambientp (process-parent action))) '()
    (let ((mover (process-parent action))
          (target-name (capability-name (action-capability action)))
          (evolutions '()))
      (flet ((entry-allower-p (x)
               (and (typep x 'action)
                    (typep (action-capability x) 'in$)
                    (eql target-name
                         (capability-name (action-capability x))))))
        (dolist (sibling (process-siblings mover) evolutions)
          (when (amb::ambientp sibling target-name)
            (dolist (p (composition-processes sibling))
              (when (entry-allower-p p)
                (let ((p p)
                      (sibling sibling))
                  (push #'(lambda ()
                            (amb::deprefix action)
                            (amb::deprefix p)
                            (amb::move-ambient mover sibling))
                        evolutions))))))))))

(defmethod action-evolutions (($out $out) (action action))
  ;; m[n[$out m.P] | out$ m.Q] => n[P] | m[Q]
  (let ((n (process-parent action))
        (m (capability-name $out))
        (evolutions '()))
    (flet ((out-allower-p (x)
             (and (typep x 'action)
                  (typep (action-capability x) 'out$)
                  (eql m (capability-name (action-capability x))))))
      (when (and (amb::ambientp n)
                 (amb::ambientp (process-parent n) m))
        (dolist (sibling (process-siblings n) evolutions)
          (when (out-allower-p sibling)
            (let ((sibling sibling))
              (push #'(lambda ()
                        (amb::deprefix action)
                        (amb::deprefix sibling)
                        (amb::move-ambient
                         n (process-parent (process-parent n))))
                    evolutions))))))))

(defmethod action-evolutions (($open $open) (action action))
  ;; $open n.P | n[open$ n.Q] => P | Q
  (let ((n (capability-name $open))
        (evolutions '()))
    (flet ((open-allower-p (x)
             (and (typep x 'action)
                  (typep (action-capability x) 'open$)
                  (eql n (capability-name (action-capability x))))))
      (dolist (sibling (process-siblings action) evolutions)
        (when (amb::ambientp sibling n)
          (dolist (p (composition-processes sibling))
            (when (open-allower-p p)
              (let ((p p)
                    (sibling sibling))
                (push #'(lambda ()
                          (amb::deprefix action)
                          (amb::deprefix p)
                          (change-class sibling 'composition))
                      evolutions)))))))))

(defexample safe-ambients-example-1
  (amb 'n ($in 'm (named 'P)))
  (amb 'm (in$ 'm (named 'Q))))

(defexample safe-ambients-example-2
  (amb 'm
    (amb 'n ($out 'm (named 'P)))
    (out$ 'm (named 'Q))))

(defexample safe-ambients-example-3
  ($open 'n (named 'P))
  (amb 'n
    (open$ 'n (named 'Q))
    (named 'R)))

;;; Miscellaneous Examples

;; Computing the product of the leaves of a binary tree

(defun tree-reduce (leaf-fn node-fn tree result)
  (if (atom tree)
    (amb result (output (funcall leaf-fn tree)))
    (new (m)
      (amb result
        (input x (input y (output (funcall node-fn x y))))
        (open m) (fun (tree-reduce leaf-fn node-fn (first tree) m))
        (open m) (fun (tree-reduce leaf-fn node-fn (second tree) m))))))

(defexample tree-reduce-example-1
  (tree-reduce '- '+
               '((1 2) (3 (4 5)))
               'result))

(defexample tree-reduce-example-2
  (tree-reduce 'list 'nconc
               '((1 2) (3 (4 5)))
               'result))

;; A parallelized propositional model finder

(defun %find-model (sentence pos neg posp sentences succeed fail)
  (destructuring-bind (op &rest args)
      (if (listp sentence) sentence
        (list sentence))
    (case op
      ((or)
       (if posp
         ;; a positive disjunction spawns a number of processes,
         ;; testing the possible disjuncts.
         (new (inner-fail)
           (labels ((disjoin (args failer)
                      (if (endp args) failer
                        (par (disjoin (rest args)
                                      (open inner-fail failer))
                             (fun (%find-model (first args)
                                               pos neg t
                                               sentences
                                               succeed inner-fail))))))
             (disjoin args (amb fail))))
         ;; a negative disjunction falsifies all its disjuncts
         (%find-model `(not ,(first args))
                      pos neg t
                      (nconc (loop for a in args
                                   collect `(not ,a))
                             sentences)
                      succeed fail)))
      ;; negation swaps posp for the negate
      ((not)
       (%find-model (first args)
                    pos neg (not posp)
                    sentences
                    succeed fail))
      ;; (and x1 ... xn) becomes (not (or (not x1) ... (not xn)))
      ((and)
       (%find-model `(not (or ,@(loop for a in args collect `(not ,a))))
                    pos neg posp
                    sentences
                    succeed fail))
      ;; (if x y) becomes (or (not x) y)
      ((if)
       (%find-model `(or (not ,(first args))
                         ,(second args))
                    pos neg posp
                    sentences
                    succeed fail))
      ;; (iff x y) becomes (and (if x y) (if y x))
      ((iff)
       (%find-model `(and (if ,(first args) ,(second args))
                          (if ,(second args) ,(first args)))
                    pos neg posp
                    sentences
                    succeed fail))
      ;; atoms 
      (otherwise
       (let ((atom (list* op args)))
         (if (member atom (if posp neg pos) :test 'equal) (amb fail)
           (if (endp sentences)
             (amb succeed
               (output
                (list (if posp (pushnew atom pos :test 'equal) pos)
                      (if posp neg (pushnew atom neg :test 'equal)))))
             (%find-model (first sentences)
                          (if posp (pushnew atom pos :test 'equal) pos)
                          (if posp neg (pushnew atom neg :test 'equal))
                          t (rest sentences) succeed fail))))))))

(defun find-model (sentence &rest args)
  (apply 'run-process
         (par 
          (%find-model sentence '() '() t '() 'succeed 'fail)
          (open 'succeed (input model (return-from find-model
                                        model))))
         args))
    
       
