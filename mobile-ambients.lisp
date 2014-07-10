;;==================================================================;;
;; Joshua Taylor
;; February 2011
;; tayloj@cs.rpi.edu
;;==================================================================;;

(defpackage #:mobile-ambients
  (:nicknames #:amb)
  (:use "COMMON-LISP")
  (:shadow #:open)
  (:documentation "The mobile-ambients package exports symbols that
comprise an interface to a mobile ambient library.  Various kinds of
{defclass amb::process} are implemented as CLOS classes, including
{defclass amb::composition} , {defclass amb::ambient} ,
{defclass amb::inactive} , processes encapsulating arbitrary Lisp
{defclass amb::computation} , and of course, {defclass amb::action}
processes prefixed with one or more {defclass amb::capability}
objects.  Once a process is constructed, it can be run with
{defun amb::run-process} .  New types of {defclass amb::process} may
be defined by including {defclass amb::process} as a superclass, and
defining an {defgeneric amb::evolutions} method to specify how it can
evolve.")
  (:export ; capabilities
   #:capability
   #:capability-name
   #:in #:in-name
   #:out #:out-name
   #:open #:open-name
   #:in^
   #:out^
   #:open^)
  (:export ; processes
   #:process #:process-parent
   #:inactive
   #:named #:named-name
   #:composition #:composition-processes
   #:ambient #:ambient-name #:ambient-processes
   #:action #:action-capability #:action-process
   #:computation #:computation-function
   #:input #:input-process-matrix
   #:output #:output-capability
   #:process-children #:process-siblings
   #:cap #:apply-capability
   #:in
   #:out
   #:open
   #:new
   #:zero
   #:named
   #:par
   #:amb
   #:make-fun
   #:fun
   #:make-input
   #:input
   #:output)
  (:export ; simulations
   #:cleanup
   #:evolutions
   #:action-evolutions
   #:run-process)
  (:export ; concurrency
   #:manager
   #:mr-parent
   #:mr-managee
   #:mr-lock
   #:ambient-manager
   #:mr-name
   #:mr-agents
   #:mr-ambients
   #:mr-inputs
   #:mr-outputs
   #:wait
   #:with-lock-notifying
   #:wait-for-parent
   #:insert-into
   #:remove-from
   #:launch-process)
  (:export ; printing
   #:precedence #:*precedence*
   #:write-process
   #:*mobile-ambient-pprint-dispatch*))

(defpackage #:mobile-ambients-user
  (:documentation "The mobile-ambient-user package USEs
mobile-ambients and COMMON-LISP, and may be useful for testing and
interactive development.  The library of examples are all defined in
the mobile-ambient-user package.")
  (:use #:mobile-ambients "COMMON-LISP")
  (:nicknames #:amb-user)
  (:shadowing-import-from #:mobile-ambients
   #:open))

(in-package #:mobile-ambients)

#|============================================================================;;
;; Docstrings in this library are formatted for CLDOC, and the
;; following form will extract documentation into the directory "doc"
;; (in the same directory as this file) if CLDOC is available and has
;; been loaded.
 
(asdf:oos 'asdf:load-op 'cldoc)
(cldoc:extract-documentation
 'cludg:html
 (namestring
  (make-pathname
   :directory (append (pathname-directory *load-truename*) '("doc"))))
 (list *load-truename*)
 :filter 'cludg::default-filter
 :section-names (list* "Syntax:"
                       "Examples:"
                       "Description:"
                       cludg::+default-section-names+))
;;============================================================================|#

;;;; Utilities

(defun random-nth (list)
  "Returns a random element from list."
  (declare (list list))
  (assert (not (endp list)))
  (nth (random (length list)) list))

(defun to-list (x)
  "Returns x if x is a list, else returns \(list x)."
  (if (listp x) x
    (list x)))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defmacro let-prog1 (bindings &body body)
  `(let ,bindings 
     (prog1 ,(caar (last bindings))
       ,@body)))

;;;; Capabilities

(defclass capability ()
  ((name
    :type symbol
    :initarg :name
    :reader capability-name
    :documentation "The ambient name associated with the capability."))
  (:documentation "Capabilities are objects that can be prefixed to
processes, producing actions.  Though capabilities are often spoken of
as granting some permission to a process, capabilites also cause the
process to block until the permitted act is performed \(though other
processes within the enclosing ambient may continue to run). "))

(defclass in (capability)
  ((name :reader in-name))
  (:documentation "In capabilities require a process's enclosing
ambient to enter an ambient with the specified name before
the process may proceed."))

(defclass out (capability)
  ((name :reader out-name))
  (:documentation "Out capabilities require a process's enclosing
ambient to enter leave an ambient with the specified name before the
process may proceed."))

(defclass open (capability)
  ((name :reader open-name))
  (:documentation "Open capabilities require that a sibling of the
capable process that is an ambient with the specified name be opened
before the capable process may proceed."))

(defun in^ (name)
  "* Syntax:
in^ name => in-capability
* Arguments and Values:
- name---an ambient name
- in-capability---an in capability
* Description:
Return an in capability with the specified name."
  (make-instance 'in :name name))

(defun out^ (name)
  "* Syntax:
out^ name => out-capability
* Arguments and Values:
- name---an ambient name
- out-capability---an out capability
* Description:
Return an out capability with the specified name."
  (make-instance 'out :name name))

(defun open^ (name)
  "* Syntax:
open^ name => open-capability
* Arguments and Values:
- name---an ambient name
- open-capability---an open capability
* Description:
Return an open capability with the specified name."
  (make-instance 'open :name name))

;;;; Processes

(defclass process ()
  ((parent
    :type process
    :initarg :parent
    :accessor process-parent
    :documentation "The parent slot of a process is either unbound or
another process, typically a composition, an ambient, or an action."))
  (:documentation "Processes correspond to mobile ambient expressions.
Each process is either top-level \(in which case its parent slot is
unbound) or a child process or a composition \(of which ambients are
subclasses) or an action."))
  
(defclass inactive (process) ()
  (:documentation "An inactive process is the null process. This is a
class rather than a priviliged individual because it may be useful to
distinguish particular inactive processes.
* See Also:
- {defun amb::zero}"))

(defclass named (process)
  ((name
    :type symbol
    :initarg :name
    :reader named-name
    :documentation "This class is used for working out examples."))
  (:documentation "The named process is useful for working out
theoretical examples and for debugging purposes.  A named process is
is printed using its name, but never evolves.
* See Also:
- {defun amb::named}"))

(defclass composition (process)
  ((processes
    :type list ; of processes
    :initarg :processes
    :accessor composition-processes
    :documentation "A list of processes in the parallel composition."))
  (:documentation "A composition is a parallel composition of
processes.  Each process in the composition has the composition as its
parent, and all the processes in the composition as its siblings.
Parallel composition is also provided by {defclass amb::ambient} .
* See Also:
- {defun amb::par}"))

(defmethod initialize-instance :after ((composition composition)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  "When a composition is created, ensure that its composed processes
have the composition as their parents."
  (dolist (process (composition-processes composition))
    (setf (process-parent process) composition)))

(defclass ambient (composition)
  ((name
    :initarg :name
    :reader ambient-name
    :documentation "The name of the ambient.  Ambient names are
arbitrary Lisp objects that will be compared with eql.")
   (processes
    :accessor ambient-processes))
  (:documentation "Ambients are named locations in which processes may
be composed in parallel.  Ambients are genuine compositions, but,
since they are identified by names, can also be entered, exited,
opened, and can enter and exit other ambients when processes with
specific capabilities are present.
* See Also:
- {defun amb::amb}"))

(defun ambientp (process &optional (name nil namep))
  "* Syntax:
ambientp process [name] => generalized-boolean
* Arguments and Values:
- process---a {defclass amb::process}
- name---an object
* Description:
If name is not provided, ambientp returns true if process is an
{defclass amb::ambient} and false otherwise.  If name is provided,
ambientp returns true when process is an ambient and its name is eql
to name."
  (and (typep process 'ambient)
       (or (not namep)
           (eql (ambient-name process) name))))

(defclass action (process)
  ((capability
    :type capability
    :initarg :capability
    :reader action-capability
    :documentation "The capability that must be realized before the
action's process may proceeed.")
   (process
    :type process
    :initarg :process
    :reader action-process
    :documentation "The process that is blocked until the capability
is realized."))
  (:documentation "Actions are processes prefixed with capabilities.
The process is blocked until the capability can be realized.
* See Also:
- {defun amb::in} , {defun amb::out} ,
{defun amb::open} , {defun amb::cap}"))

(defclass computation (process)
  ((function
    :type function
    :initarg :function
    :reader computation-function
    :documentation "The function to be invoked that allows the process
to continue.  When the process evolves by invocation of this function,
if the result of the function is another process, that process
replaces the computation.  Otherwise, an inactive process replaces the
computation."))
  (:documentation "A computation process is an extension to the theory
of mobile ambients, and is the key way in which Common Lisp is
integrated into mobile ambient processes.  A computation process
evolves by invoking the computation's function.  If the result of this
invocation is a process, the computation evolves as the result,
otherwise as an {defclass amb::inactive} process.
* See Also:
- {defmacro amb::fun} , {defun amb::make-fun}"))

(defclass input (process)
  ((process-matrix
    :type function
    :initarg :process
    :reader input-process-matrix
    :documentation "A function of one argument, a capability, that
should return a process.  When an input value becomes available, this
functin is invoked with the value, and the input process evolves as
the new process."))
  (:documentation "Input processes read a value from some sibling
output process, and evolve as a new process in which the value is
available.  Internally, the input process has a function of one
argument which is invoked with a received value when one becomes
available. The function should produces a process, and it as this
process that the input process evolves.
* See Also:
- {defmacro amb::input} , {defun amb::make-input}"))

(defclass output (process)
  ((capability
    :initarg :capability
    :reader output-capability
    :documentation "The capability that will be output by the output
process, either a capability object, or an ambient name."))
  (:documentation "Output processes emit a value when an input process
is available within the same enclosing composition and then evolve to
an inactive process \(output in mobile ambients is asynchronous).
* See Also:
- {defun amb::output}"))

(defgeneric process-children (process)
  (:documentation "* Syntax:
process-children process => children
* Arguments and Values:
- process---a {defclass amb::process}
- children---a list of processes
* Description:
Process-children returns the children of process that can be currently
running within process.  For ambients and compositions, this is simply
the list of composed processes, for others, the empty list.")
  (:method ((composition composition))
   (composition-processes composition))
  (:method ((process process))
   '()))

(defun process-siblings (process)
  "* Syntax:
process-siblings process => siblings
* Arguments and Values:
- process---a {defclass amb::process}
- siblings---a list of processes
* Description:
Returns the children of process's parent.  No check is made to ensure
that process's parent is bound or a process."
  (process-children (process-parent process)))

;; Process Constructors

(defun cap (capability process)
  "An abbreviation for {defun amb::apply-capability} ."
  (apply-capability capability process))

(defun apply-capability (capability &optional (process (zero)))
  "* Syntax:
apply-capability capability process => result
* Arguments and Values:
- capability---a {defclass amb::capability} or list of capabilities
- process---a {defclass amb::process}
- result---an {defclass amb::action} process
* Description:
Returns process prefixed by the given capability or capabilities in
the order that they are provided.  I.e., the last capability is
prefixed to process, then the next to last capability is prefixed to
that, and so on.  If no capabilities are provided, process is
returned directly.
* Notes:
In the mobile ambients theory, capability paths are a kind of
capability.  We do not introduce capability paths, opting to use lists
of capabilities instead."
  (flet ((mkaction (capability process)
           (let-prog1 ((action (make-instance
                                'action
                                :capability capability
                                :process process)))
             (setf (process-parent process) action))))
    (reduce #'mkaction (to-list capability)
            :from-end t :initial-value process)))

(defun in (name &optional (process (zero)))
  "* Syntax:
in name [process] => action
* Arguments and Values:
- name---an ambient name
- process---a {defclass amb::process} , the default is an inactive process
- action---an {defclass amb::action} process
* Description:
In returns an action process with an in capability with the given name
and process.  If no process is provided, an inactive process is
created by \(zero)."
  (cap (in^ name) process))

(defun out (name &optional (process (zero)))
  "* Syntax:
out name [process] => action
* Arguments and Values:
- name---an ambient name
- process---a {defclass amb::process} , the default is an inactive process
- action---an {defclass amb::action} process
* Description:
Out returns an action process with an out capability with the given name
and process.  If no process is provided, an inactive process is
created by \(zero)."
  (cap (out^ name) process))

(defun open (name &optional (process (zero)))
  "* Syntax:
open name [process] => action
* Arguments and Values:
- name---an ambient name
- process---a {defclass amb::process} , the default is an inactive process
- action---an {defclass amb::action} process
* Description:
Open returns an action process with an open capability with the given name
and process.  If no process is provided, an inactive process is
created by \(zero)."
  (cap (open^ name) process))

(defmacro new (names &body process)
  "* Syntax:
new (var*) declaration* form* => result*
* Arguments and Values:
- var---a symbol naming a variable not evaluated
- declaration---a declare expression, not evaluated
- form---a form
- results---the values returned by the forms
* Description:
New binds the variables to new symbols that can be used within forms.
Since ambient names are Lisp objects comparable via eql, fresh symbols
serve as suitable, fresh, new names for ambients."
  (if (endp names) process
    (let ((bindings '()))
      (dolist (name names (setf bindings (nreverse bindings)))
        (destructuring-bind (var &optional (form `(quote ,var)))
            (if (listp name) name (list name))
          (push `(,var (make-symbol (princ-to-string ,form))) bindings)))
      `(let ,bindings
         ,@process))))

(defun zero ()
  "* Syntax:
zero => inactive-process
* Arguments and Values:
- inactive-process---a new {defclass amb::inactive} process"
  (make-instance 'inactive))

(defun named (name)
  "* Syntax:
named name => named-process
* Arguments and Values:
- name---an object
- named-process---a new {defclass amb::named} process named name"
  (make-instance 'named :name name))

(defun par (&rest processes)
  "* Syntax:
par process* => composition
* Arguments and Values:
- process---a {defclass amb::process}
- composition---a {defclass amb::composition} of the processes"
  (make-instance 'composition :processes processes))

(defun amb (name &rest processes)
  "* Syntax:
amb name process* => ambient
* Arguments and Values:
- name---an object
- process---a {defclass amb::process}
- ambient---an {defclass amb::ambient}
* Description:
amb returns an ambient named named containing the processes."
  (make-instance 'ambient :name name :processes processes))

#+lispworks (editor:setup-indent "amb" 1)

(defun make-fun (function)
  "* Syntax:
make-fun function => computation
* Arguments and Values:
- function---a function of no arguments
- computation---a {defclass amb::computation}
* Description:
make-fun returns a computation whose function is function."
  (make-instance 'computation :function function))

(defmacro fun (&body body)
  "* Syntax:
fun [[declaration* | documentation]] form* => computation
* Arguments and Values:
- declaration---a declare expression; not evaluated
- documentation---a documentation string; not evaluated
- forms---forms
- computation---a {defclass amb::computation}
* Description:
fun returns a computation whose function is a function with the body
of fun.
;;; (fun ...) === (make-fun #'(lambda () ...))
* See Also:
- {defun amb::make-fun}"
  `(make-fun #'(lambda () ,@body)))

(defun make-input (function)
  "* Syntax:
make-input function => input
* Arguments and Values:
- function---a function of one argument
- input---an {defclass amb::input} process
* Description:
make-input returns a input process with the specified function."
  (make-instance 'input :process function))

(defmacro input (name &body body)
  "* Syntax:
input name [[declaration* | documentation]] form* => input
* Arguments and Values:
- name---a symbol naming a variable
- declaration---a declare expression; not evaluated
- documentation---a documentation string; not evaluated
- forms---forms
- input---an {defclass amb::input} process
* Description:
input produces an input process whose function has the specified body,
and within whose body name is bound to the received input value.
;;; (input x ...) === (make-input #'(lambda (x) ...))
* See Also:
- {defun amb::make-input}"
  (if (not (listp name))
    `(make-input #'(lambda (,name) ,@body))
    (let ((arglist (gensym (string '#:arglist-))))
      `(make-input #'(lambda (,arglist)
                       (apply #'(lambda ,name ,@body)
                              ,arglist))))))

(defun output (capability)
  "* Syntax:
output capability => output
* Arguments and Values:
- capability---an object
- output---an {defclass amb::output} process
* Description:
output returns an output process that outputs the specified
capability, which is expected to be a capability, a list of
capabilities, or an object \(an ambient name)."
  (make-instance 'output :capability capability))

;;;; Pretty Printing

(defparameter *mobile-ambient-pprint-dispatch*
  (copy-pprint-dispatch)
  "A pprint-dispatch-table used for pretty printing processes and
capabilities.  print-object methods are defined for the standard
{defclass amb::process} and {defclass amb::capability} object that
pretty print the object to the stream \(within
print-unreadable-object) using this pprint dispatch.
* See Also:
- {defgeneric amb::write-process}
- {defgeneric amb::precedence}")

(defgeneric write-process (stream process)
  (:documentation "* Syntax:
write-process stream process =>
* Arguments and Values:
- stream---an output stream
- process---a {defclass amb::process} 
* Description:
write-process is responsible for writing a pretty representation of
the process to stream.  write-process is installed as the
pprint-dispatch in {defparameter amb::*mobile-ambient-pprint-dispatch*}
for {defclass amb::capability} and {defclass amb::process} objects.
In turn, print-object methods have been defined for these same classes
that write the objects to a stream \(within print-unreadable-object)
using the mobile ambient pprint dispatch table.  When defining a new
type of process or capability, defining appropriate write-process
methods should make the new processes print nicely.  There is a
default :around method that, in conjunction with
{defgeneric amb::precedence} ensure that parentheses are printed
around processes where appropriate.
* See Also:
- {defgeneric amb::precedence}
- {defparameter amb::*mobile-ambient-pprint-dispatch*}")
  (:method (stream (capability capability))
   (format stream "{capability} ~:w"
           (capability-name capability)))
  (:method (stream (process process))
   (format stream "{process}")))

(set-pprint-dispatch 
 'capability 'write-process 0
 *mobile-ambient-pprint-dispatch*)

(defmethod write-process (stream (in in))
  (format stream "in ~:w" (capability-name in)))

(defmethod write-process (stream (out out))
  (format stream "out ~:w" (capability-name out)))

(defmethod write-process (stream (open open))
  (format stream "open ~:w" (capability-name open)))

(defmethod print-object ((c capability) stream)
  (print-unreadable-object (c stream)
    (write c
           :stream stream
           :pretty t
           :pprint-dispatch *mobile-ambient-pprint-dispatch*)))

(set-pprint-dispatch
 'process 'write-process 0
 *mobile-ambient-pprint-dispatch*)

(defparameter *precedence* 0
  "The precedence of the process being printed by
write-process.  Precedence values are automatically computed
by {defgeneric amb::precedence} and bound within an :around
method on {defgeneric amb::write-process} . It is not
expected that this value would be manually modified. The
initial value is 0.")

(defgeneric precedence (process)
  (:documentation "* Syntax:
precedence process => precedence, ambiguousp
* Arguments and Values:
- process---a {defclass amb::process}
- precedence---a positive number
- ambiguousp---a boolean
* Description:
precedence returns enough information to, in conjunction with the
value of {defparameter amb::*precedence*} , determine whether a pretty
printing of process should be surrounded by parentheses.  If a
process's precedence is less than its parent's, or if it has the same
precedence, but ambigousp is true, then the process is surrounded by
parentheses.
  Methods are defined for all of the built in processes, and have the
following behavior. For {defclass amb::composition} processes,
precedence returns 4 and false if the composition has at most one
process, and 2 and true otherwise. For all of the following classes,
precedence returns 4 and true.
- {defclass amb::inactive}
- {defclass amb::named}
- {defclass amb::ambient}
- {defclass amb::computation}
- {defclass amb::input}
- {defclass amb::output}
- {defclass amb::action}
* See Also:
- {defgeneric amb::write-process}
- {defparameter amb::*mobile-ambient-pprint-dispatch*}
- {defparameter amb::*precedence*}")
  (:method ((p process))     (values 0 t))
  (:method ((i inactive))    (values 4 nil))
  (:method ((n named))       (values 4 nil))
  (:method ((a ambient))     (values 4 nil))
  (:method ((c computation)) (values 4 nil))
  (:method ((i input))       (values 4 nil))
  (:method ((o output))      (values 4 nil))
  (:method ((a action))      (values 4 nil))
  (:method ((c composition))
   (if (endp (rest (composition-processes c)))
     (values 4 nil)
     (values 2 t))))

(defmethod write-process :around (stream (p process))
  (multiple-value-bind (precedence ambiguousp) (precedence p)
    (let* ((surroundp (or (< precedence *precedence*)
                          (and ambiguousp
                               (= precedence *precedence*))))
           (*precedence* precedence))
      (when surroundp (write-char #\( stream))
      (prog1 (call-next-method)
        (when surroundp (write-char #\) stream))))))

(defmethod write-process (stream (i inactive))
  (declare (ignore i))
  (write-char #\0 stream))

(defmethod write-process (stream (n named))
  (format stream "~:w" (named-name n)))

(defmethod write-process (stream (c composition))
  (pprint-logical-block (stream (composition-processes c))
    (pprint-exit-if-list-exhausted)
    (loop 
     (write (pprint-pop) :stream stream)
     (pprint-exit-if-list-exhausted)
     (write-string " | " stream)
     (pprint-newline :linear stream))))

(defmethod write-process (stream (a ambient))
  (format stream "~:w[" (ambient-name a))
  (prog1 (call-next-method)
    (write-char #\] stream)))

(defmethod write-process (stream (a action))
  (let ((c (action-capability a))
        (p (action-process a)))
    (if (typep p 'inactive)
      (format stream "~:w" c)
      (format stream "~:w.~:w" c p))))

(defmethod write-process (stream (c computation))
  (declare (ignore c))
  (format stream "{proc}"))

(defmethod write-process (stream (i input))
  (declare (ignore i))
  (format stream "().#"))

(defmethod write-process (stream (o output))
  (format stream "{~:w}"
          (output-capability o)))

(defmethod print-object ((p process) stream)
  (print-unreadable-object (p stream)
    (write p
           :stream stream
           :pretty t
           :pprint-dispatch *mobile-ambient-pprint-dispatch*)))

;;;; Running Processess Concurrently

(defclass manager ()
  ((parent 
    :initform nil
    :type (or null manager)
    :accessor mr-parent
    :documentation "The parent of the manager.")
   (managee
    :type process
    :initarg :managee
    :reader mr-managee
    :documentation "The process being managed.")
   (lock
    :type mp:lock
    :reader mr-lock
    :initform (mp:make-lock :name "manager lock")
    :documentation "A lock for the managed process.")
   (changedp
    :type mp::condition-variable
    :reader mr-changedp
    :initform (mp:make-condition-variable :name "manager changed")
    :documentation "A condition variable signalled when manager changes."))
  (:documentation
   "For each conceptual process in program, a manager records the
process's parent and the managed process expression.  Each manager has
a lock which should be obtained before modifying the manager, and a
conditional variable on which threads may wait for the manager to
change."))

(defclass ambient-manager (manager)
  ((name
    :type t
    :initarg :name
    :reader mr-name
    :documentation "The name of the managed object.")
   (agents
    :type list ; of (non ambient-)managers
    :initform '()
    :accessor mr-agents
    :documentation "A list of the agents managed by the ambient.")
   (ambients
    :type list ; of ambient-managers
    :initform '()
    :accessor mr-ambients
    :documentation "A list of the ambients contained in the ambient.")
   (inputs
    :type list ; of input processes
    :initform '()
    :accessor mr-inputs
    :documentation "A list of the input processes in ambient.")
   (outputs
    :type list ; of output processes
    :initform '()
    :accessor mr-outputs
    :documentation "A list of the output processes in ambient.")
   (openedp 
    :type boolean
    :initform nil
    :accessor mr-openedp
    :documentation "True if the ambient has been opened."))
  (:documentation
   "Ambients, as processes, are managed, but also serve as arenas for
computation.  An ambient has a name, subprocesses, subambients, input
and output processes."))

(defmethod print-object ((am ambient-manager) stream)
  (print-unreadable-object (am stream :type t :identity t)
    (format stream "~w[...]" (mr-name am))))

;; The following code is loosely based on the Java code given in
;; Cardelli's "Mobile Ambient Syncronization" (MAS), SRC Technical
;; Note 1997--013.  Some methods (move-in, move-out, and open-up) are
;; more compact here since there is no need to implement different
;; parts of the moves as multiple methods.  Wait-for-parent is
;; slightly more complicated here to account for openining---i.e.,
;; once an ambient is opened, it will never again have a parent.

(defun wait (manager &optional wait-reason)
  "* Syntax: 
wait manager &key wait-reason => wakep
* Arguments and Values:
- manager---a manager
- wait-reason---a string, or nil
* Description:
Wait \(using mp:condition-variable-wait\) for the manager's changedp
condition variable."
  (mp:condition-variable-wait
   (mr-changedp manager) (mr-lock manager)
   :wait-reason wait-reason))

(defmacro sending-notification ((manager &rest args) &body body)
  "* Syntax:
sending-notification manager-form form* => result*
* Arguments and Values:
- manager-form---a form, evaluated to produce a manager
- forms---forms
- results---the results of the forms
* Description:
with-lock-modifying locks the manager's lock, evaluates the body, and
broadcasts on the manager's changedp condition variable \(thus waking
any threads waiting on it\)."
  (let ((m (gensym (string '#:manager-))))
    `(let ((,m ,manager))
       (mp:with-lock ((mr-lock ,m) ,@args)
         (unwind-protect (progn ,@body)
           (mp:condition-variable-broadcast
            (mr-changedp ,m)))))))

(defun wait-for-parent (manager)
  "* Syntax:
wait-for-parent manager => parent
* Arguments and Values:
- manager, parent---managers
* Description:
Waits until either the manager has a non-nil parent or the managed
ambient \(if the manager is an ambient manager\) is opened.  When
either case holds, the parent of the manager is returned.  In the
latter case, the parent of the manager is nil, and thus nil is
returned. The caller is responsible for ensuring that manager is
actually a child of the returned parent when then parent is used."
  (mp:with-lock ((mr-lock manager) "wait-for-parent waiting for change")
    (do ((ambientp (typep manager 'ambient-manager))
         (parent (mr-parent manager) (mr-parent manager)))
        ((or (not (null parent))
             (and ambientp (mr-openedp manager)))
         parent)
      (wait manager "wait-for-parent waiting for a change"))))

(defun insert-into (manager new-parent)
  "* Syntax:
insert-into manager new-parent
* Arguments and Values:
- manager---a manager
- new-parent---an ambient-manager
* Description:
insert-into sets the manager's parent to new-parent and adds manager
to new-parent's list of ambients if manager is an ambient-manager or
else to new-parent's list of agents."
  (sending-notification (manager "insert-into manager")
    (setf (mr-parent manager) new-parent)
    (sending-notification (new-parent "insert-into new-parent")
      (etypecase manager
        (ambient-manager (push manager (mr-ambients new-parent)))
        (manager         (push manager (mr-agents new-parent)))))))

(defun remove-from (manager old-parent)
  "* Syntax:
remove-from manager new-parent
* Arguments and Values:
- manager---a manager
- new-parent---an ambient-manager
* Description:
remove-from sets the manager's parent to nil and deletes manager from
new-parent's list of ambients if manager is an ambient-manager or else
from new-parent's list of agents."
  (sending-notification (old-parent "remove-from old-parent")
    (etypecase manager
      (ambient-manager (setf (mr-ambients old-parent)
                             (delete manager (mr-ambients old-parent))))
      (manager         (setf (mr-agents old-parent)
                             (delete manager (mr-agents old-parent)))))
    (sending-notification (manager "remove-from manager")
      (setf (mr-parent manager) nil))))

(defun move-out (agent)
  "* Syntax:
move-out agent => nil
* Arguments and Values:
- agent---a manager for an action with an out capability
* Description:
move-out moves agent's containing ambient, n, out of n's containing
ambient, m, \(when m is the name in the out capability\) and into the
m's parent.  In ambient terms, given a situation m[n[out m.P]], agent
is the manager for the process \"out m.P\", move-out accomplishes the
transition to m[] | n[P]."
  (loop with action = (mr-managee agent)
        with out = (action-capability action)
        with K = (action-process action)
        ;; Grab the ambient, the parent, and the grandparent.  These
        ;; aren't locked yet, so the hierarchy can change before these
        ;; are used.  The proper child-parent relationships will be
        ;; checked as the locks are established.
        for ambient = (wait-for-parent agent)
        for parent = (wait-for-parent ambient)
        for grandparent = (wait-for-parent parent)
        ;; The ambients are locked in the following order: grandparent,
        ;; parent, ambient.  However, they are released in a different
        ;; order.  If the hierarchy is proper and the parent has the
        ;; proper name, then parent is unlocked as soon as the inner
        ;; ambient is removed from it, and the other locks are unlocked
        ;; in the reverse order.  If the hierarchy is proper but the
        ;; parent has the wrong name, then the grandparent and parent
        ;; locks are released but the ambient lock is held until a
        ;; change in the ambient occurs.
        unless (or (null ambient) (null parent) (null grandparent)) do
        (mp:process-lock (mr-lock grandparent) "move-out grandparent")
        (cond
         ((not (member parent (mr-ambients grandparent)))
          (mp:process-unlock (mr-lock grandparent)))
         (t 
          (mp:process-lock (mr-lock parent) "move-out parent")
          (cond
           ((not (member ambient (mr-ambients parent)))
            (mp:process-unlock (mr-lock parent))
            (mp:process-unlock (mr-lock grandparent)))
           (t
            (mp:process-lock (mr-lock ambient) "move-out ambient")
            (cond
             ((not (member agent (mr-agents ambient)))
              (mp:process-unlock (mr-lock ambient))
              (mp:process-unlock (mr-lock parent))
              (mp:process-unlock (mr-lock grandparent)))
             ((not (eql (out-name out) (mr-name parent)))
              (mp:process-unlock (mr-lock parent))
              (mp:process-unlock (mr-lock grandparent))
              (wait ambient "move-out waiting for ambient change")
              (mp:process-unlock (mr-lock ambient)))
             (t
              (remove-from ambient parent)
              (mp:process-unlock (mr-lock parent))
              (insert-into ambient grandparent)
              (mp:process-unlock (mr-lock grandparent))
              (remove-from agent ambient)
              (launch-process K ambient)
              (mp:process-unlock (mr-lock ambient))
              (return)))))))))

(defun move-in (agent)
  "* Syntax:
move-in agent => nil
* Arguments and Values:
- agent---a manager for an action with an in capability
* Description:
move-in moves agent's containing ambient, n, into a sibling m, \(when
m is the name in the in capability\).  In ambient terms, given a
situation n[in m.P] | m[], agent is the manager for the process \"in
m.P\", move-in accomplishes the transition to m[n[P]]."
  ;; The parent of the ambient containing the agent is locked first,
  ;; and then the ambient containing the agent.  When the topology is
  ;; correct and a sibling with a suitable name can be found, the
  ;; topology is modified accordingly.  If the topology is OK, but no
  ;; sibling can be found, then the ambient lock is released, but the
  ;; process waits until some change is detected in the parent.
  (let* ((action (mr-managee agent))
         (in (action-capability action))
         (K (action-process action)))
    (do (ambient parent hierarchyp sibling) (sibling)
      (setf hierarchyp nil
            ambient (wait-for-parent agent)
            parent (wait-for-parent ambient))
      (unless (or (null ambient) (null parent))
        (mp:with-lock ((mr-lock parent) "move-in parent")
          (when (member ambient (mr-ambients parent))
            (mp:with-lock ((mr-lock ambient) "move-in ambient")
              (when (member ambient (mr-ambients parent))
                (setf hierarchyp t
                      sibling (find-if #'(lambda (a)
                                           (and (not (eq a ambient))
                                                (eql (mr-name a)
                                                     (in-name in))))
                                       (mr-ambients parent)))
                (unless (null sibling)
                  (mp:with-lock ((mr-lock sibling) "move-in sibling")
                    (remove-from ambient parent)
                    (insert-into ambient sibling)
                    (remove-from agent ambient)
                    (launch-process K ambient))))))
          (when (and hierarchyp (null sibling))
            (wait parent "move-in waiting for parent to change")))))))

(defun open-up (agent)
  "* Syntax:
open-up agent => nil
* Arguments and Values:
- agent---a manager for an action with an open capability
* Description:
open-up opens a sibling ambient m into the agent's containing ambient
n.  In ambient terms, given a situation open m.P | m[Q], agent is the
manager for the process \"open m.P\", open-up accomplishes the
transition to P | Q."
  (loop with action = (mr-managee agent)
        with open = (action-capability action)
        with K = (action-process action)
        for parent = (wait-for-parent agent)
        unless (null parent) do
        (mp:with-lock ((mr-lock parent) "open-up parent")
          (when (member agent (mr-agents parent))
            (let ((ambient (find (open-name open)
                                 (mr-ambients parent)
                                 :key 'mr-name)))
              (cond
               ((null ambient)
                (wait parent "open-up waiting for parent change"))
               ((mp:with-lock ((mr-lock ambient) "open-up ambient")
                  (when (member ambient (mr-ambients parent))
                    (remove-from ambient parent)
                    (relocate-contents ambient parent)
                    (sending-notification (ambient)
                      (setf (mr-openedp ambient) t))
                    (remove-from agent parent)
                    (launch-process K parent)
                    (return))))))))))

(defun relocate-contents (ambient1 ambient2)
  "* Syntax:
relocate-contents ambient1 ambient2 => nil
* Arguments and Values:
- ambient1, ambient2---ambient-managers
* Description:
relocate-contents moves the contents \(agents, ambients, inputs, and
outputs\) of ambient1 into ambient2."
  (do () ((endp (mr-outputs ambient1)))
    (start-output (pop (mr-outputs ambient1)) ambient2))
  (do () ((endp (mr-inputs ambient1)))
    (start-input (pop (mr-inputs ambient1)) ambient2))
  (dolist (agent (mr-agents ambient1))
    (remove-from agent ambient1)
    (insert-into agent ambient2))
  (dolist (ambient (mr-ambients ambient1))
    (remove-from ambient ambient1)
    (insert-into ambient ambient2)))

(defun execute-computation (manager)
  "* Syntax:
execute-computation manager => nil
* Arguments and Values:
- manager---a manager for a computation
* Description:
execute-computation invokes the computation's function to produce a
result.  Then the manager is removed from its parent and, if the
result is another process, the process is added to that parent."
  (loop with computation = (mr-managee manager)
        with result = (funcall (computation-function computation))
        for parent = (wait-for-parent manager)
        do (mp:with-lock ((mr-lock parent) "compute parent")
             (when (member manager (mr-agents parent))
               (remove-from manager parent)
               (when (typep result 'process)
                 (launch-process result parent))
               (return)))))

(defun start-input (input parent &aux output)
  "* Syntax:
start-input input parent
* Arguments and Values:
- input---an input process
- parent---a manager
* Description:
start-input synchronizes an input process with an output in the parent
if one is available and lauches the resulting process.  If no output
is available in the parent, the input process is added the manager's
input buffer."
  (mp:with-lock ((mr-lock parent) "start-input parent")
    (if (endp (mr-outputs parent))
      (push input (mr-inputs parent))
      (setf output (pop (mr-outputs parent))))
    (start-i/o input output parent)))

(defun start-output (output parent &aux input)
  "* Syntax:
start-output output parent
* Arguments and Values:
- output---an output process
- parent---a manager
* Description:
start-input synchronizes an output process with an input process in
the parent if one is available and lauches the resulting process.  If
no input is available in the parent, the output process is added the
manager's output buffer."
  (mp:with-lock ((mr-lock parent) "start-output parent")
    (if (endp (mr-inputs parent))
      (push output (mr-outputs parent))
      (setf input (pop (mr-outputs parent))))
    (start-i/o input output parent)))

(defun start-i/o (input output parent)
  "* Syntax:
start-i/i input output parent
* Arguments and Values:
- input---an input process, or nil
- output---an output process, or nil
* Description:
When both input and output are non-nil, start-i/o extracts the value
from the output process, calls the input process matrix with it to
produce a new process and launches the resulting process."
  (unless (or (null input) (null output))
    (launch-process
     (funcall (input-process-matrix input) 
              (output-capability output))
     parent)))

(defun manage-ambient (ambient)
  "* Syntax:
manage-ambient ambient => ambient-manager
* Arguments and Values:
- ambient---an ambient
- ambient-manager---an ambient-manager
* Description:
Returns an ambient manager for the ambient."
  (make-instance 'ambient-manager 
                 :managee ambient
                 :name (ambient-name ambient)))

(defun manage-action (action)
  "* Syntax:
manage-action action => manager
* Arguments and Values:
- action---an action
- manager---a manager
* Description:
Returns a manager for the action.  In addition, a new mp:process is
spawned that invokes move-in, move-out, or open-up \(depending on the
type of capability in the action\) with the manager."
  (let-prog1 ((mr (make-instance 'manager :managee action)))
    (flet ((manage (name function)
             (mp:process-run-function 
              name '(:mailbox nil) 
              function mr)))
      (etypecase (action-capability action)
        (in (manage "in action" 'move-in))
        (out (manage "out action" 'move-out))
        (open (manage "open action" 'open-up))))))

(defun manage-computation (computation)
  "* Syntax:
manage-computation computation => manager
* Arguments and Values:
- computation---a computation
- manager---a manager
* Description:
Returns a manager for the computation.  In addition, a new mp:process
is spawned that invokes execute-computation with the manager."
  (let-prog1 ((mr (make-instance 'manager :managee computation)))
    (mp:process-run-function 
     "computation" '(:mailbox nil)
     'execute-computation mr)))

(defgeneric launch-process (process &optional parent)
  (:documentation "* Syntax:
launch-process process &optional parent => 
* Arguments and Values:
- process---a process
- parent---an ambient-manager or nil
* Description:
launch-process starts the process in the provided parent, or on its
own if the parent is nil.  \(Some methods, i.e., those for processes
which require a parent, require that the parent argument is non-nil.\)
The value of launch-process is not used by any of the standard code,
and no requirements are imposed on the values returned by
programmer-defined methods.")
  (:method ((p inactive) &optional parent))
  (:method ((p named) &optional parent))
  (:method ((i input) &optional parent)
   (start-input i parent))
  (:method ((o output) &optional parent)
   (start-output o parent))
  (:method ((a ambient) &optional parent)
   (let ((mr (manage-ambient a)))
     (dolist (subprocess (ambient-processes a))
       (launch-process subprocess mr))
     (unless (null parent)
       (insert-into mr parent))))
  (:method ((a action) &optional parent)
   (let ((mr (manage-action a)))
     (insert-into mr parent)))
  (:method ((c computation) &optional parent)
   (let ((mr (manage-computation c)))
     (unless (null parent)
       (insert-into mr parent))))
  (:method ((c composition) &optional parent)
   (dolist (subprocess (composition-processes c))
     (launch-process subprocess parent))))

;;;; Process Reductions

(defgeneric cleanup (process)
  (:documentation "* Syntax:
cleanup process => results*
* Arguments and Values:
- process---a {defclass amb::process}
* Description:
cleanup is responsible for cleaning processes by performing trivial
transformations not significant enough to merit full evolutions.
Cleanup, for the standard process types, removes inactive processes
and from compositions \(including ambients) and moves any processes
from a non-ambient composition whose parent is a composition
\(including ambients) into its parent and removes the contained
composition.  Cleanup also cleans up subprocesses, even those which
could not progress on their own \(e.g., those with a capability prefix
that cannot currently be executed).  {defun amb::run-process} calls
cleanup before the intial computation of evolutions, and between
evolutions.
* Examples:
;;;  (setq process
;;;        (amb 'k
;;;          (in 'n (par (par (named 'P) (zero))))
;;;          (out 'm (par (named 'Q) (par) (named 'R) (par)))))
;;; => #<K[in N.(P | 0) | out M.(Q |  | R | )]>
;;; (cleanup process) => unspecified
;;; process => #<K[in N.P | out M.(Q | R)]>
* See Also:
- {defun amb::run-process}")
  (:method ((process process)) nil)
  (:method ((action action))
   (cleanup (action-process action)))
  (:method ((composition composition) &aux (new-kids '()))
   (dolist (p (composition-processes composition))
     (cleanup p)
     (cond
      ((and (typep p 'composition)
            (not (typep p 'ambient)))
       (dolist (pp (composition-processes p))
         (setf (process-parent pp) composition)
         (push pp new-kids)))
      ((not (typep p 'inactive))
       (push p new-kids))))
   (setf (composition-processes composition)
         (nreverse new-kids))))

(defgeneric evolutions (process)
  (:documentation "* Syntax:
evolutions process => evolutions
* Arguments and Values:
- process---a {defclass amb::process}
- evolutions---a list of functions of no arguments
* Description:
evolutions returns a list of functions of no arguments any of which
when invoked will \(destructively) evolve process by one step.  Since
the evolutions destructively modify process, there is no guarantee
that any of them will be appropriate to call after one of them has
been called.
  methods on evolutions have been defined for the built in types of
processes.  For instance, (evolutions ((i {defclass amb::inactive} )))
returns the empty list, since an inactive process may never evolve.
\(evolutions \(\(c {defclass amb::composition} ))) returns a list
containing all of the evolutions of its composed processes.")
  (:method ((process process)) '()))

(defmethod evolutions ((c composition))
  "A composition has no evolutions of its own, but its sub-processes
may evolve."
  (mapcan 'evolutions (composition-processes c)))

(defmethod evolutions ((action action))
  "An action's evolutions are computed by
{defgeneric amb::action-evolutions} invoked with the action's
capability and the action."
  (action-evolutions (action-capability action) action))

(defgeneric action-evolutions (capability action)
  (:documentation "* Syntax:
action-evolutions capability action => evolutions
* Arguments and Values:
- capability---a {defclass amb::capability}
- action---an {defclass amb::action} process
- evolutions---a list of functions of no arguments
* Description:
action-evolutions is called by the {defgeneric amb::evolutions} method
for actions. An action-evolutions method should be defined for each
capability type. Methods are defined for the standard capabilities
{defclass amb::in} , {defclass amb::out} , and {defclass amb::open} .
When the evolutions method for action calls action-evolutions with the
action's capability and the action, and returns its results.
* See Also:
- {defgeneric amb::evolutions}")
  (:method ((capability capability) (action action))
   '())
  (:method ((in in) (action action))
   "A process \"in x.P\" may evolve as P when it is enclosed in an
ambient one of whose proper siblings is named x \(when the enclosing
ambient moves into x)."
   (let ((parent (process-parent action)))
     (if (not (ambientp parent)) '()
       (loop with cname = (capability-name (action-capability action))
             for s in (process-siblings parent)
             unless (eq s parent)
             when (ambientp s cname)
             collect (let ((s s))
                       #'(lambda ()
                           (deprefix action)
                           (move-ambient parent s)))))))
  (:method ((out out) (action action))
   "A process \"out x.P\" may progress as P when its parent is an
ambient whose parent is an ambient named x and \"out x.P\"'s parent
moves out of the ambient x."
   (let ((parent (process-parent action)))
     (if (and (ambientp parent)
              (ambientp (process-parent parent)
                        (capability-name out)))
       (list #'(lambda ()
                 (deprefix action)
                 (move-ambient parent
                               (process-parent
                                (process-parent parent)))))
       '())))
  (:method ((open open) (action action))
   "A process \"open x.P\" may progress as P when it has as a sibling
an ambient named x \(and x is torn open)."
   (loop with name = (capability-name open)
         for sibling in (process-siblings action)
         when (ambientp sibling name)
         collect (let ((sibling sibling))
                   #'(lambda ()
                       (open-ambient action sibling))))))

(defmethod evolutions ((c computation))
  "A computation process evolves by computing."
  (list #'(lambda () (compute c))))

(defmethod evolutions ((o output))
  "An output process evolves by disappearing, but the input process
with which it synchronizes does evolve. Since these match up, we only
compute the evolutions for one of them, the output process."
  (loop with parent = (process-parent o) 
        for s in (composition-processes parent)
        when (typep s 'input)
        collect (let ((s s))
                  #'(lambda ()
                      (communicate o s)))))

(defun communicate (sender receiver)
  "Perform the communication between an output process, sender, and an
input process, receiver.  The output value is extracted from the
sender and passed to the input process's process generating matrix
which generates a process to replace the receiver.  Sender is removed
from the composition."
  (let ((parent (process-parent sender))
        (new-process (funcall (input-process-matrix receiver)
                              (output-capability  sender))))
    (setf (process-parent new-process) parent)
    (setf (composition-processes parent)
          (substitute
           new-process receiver 
           (delete sender (composition-processes parent))))))

(defun deprefix (action)
  "Turn an action a.P into P by replacing a.P with P in a.P's parent,
and setting the parent of P to be the parent of a.P."
  (setf (process-parent (action-process action))
        (process-parent action))
  (setf (composition-processes (process-parent action))
        (substitute (action-process action) action
                    (composition-processes (process-parent action)))))

(defun move-ambient (moving-ambient target-ambient)
  "Remove the prefix from action, and put moving-ambient into
target-ambient."
  ;; remove the moving-ambient from its parent
  (setf (composition-processes
         (process-parent moving-ambient))
        (delete moving-ambient
                (composition-processes
                 (process-parent moving-ambient))))
  ;; put moving-ambient into target-ambient
  (setf (process-parent moving-ambient) target-ambient)
  (push moving-ambient (composition-processes target-ambient)))

(defun open-ambient (action ambient)
  "Opening an ambient removes the prefix from the action, and turns
the ambient into a composition.  The cleanup action will then lift the
composition up into the surrounding context."
  (deprefix action)
  (change-class ambient 'composition))

(defun compute (computation)
  "Call the function of a computation process.  If the result is
process, then then new process replaces the computation process in the
parent.  Otherwise, the computation process is removed from its
parent."
  (let ((parent (process-parent computation))
        (result (funcall (computation-function computation))))
    (if (typep result 'process)
      ;; replace a computation with its resulting process.
      (setf (composition-processes parent)
            (substitute result computation
                        (composition-processes parent))
            (process-parent result)
            (process-parent computation))
      ;; remove the computation from its parent
      (setf (composition-processes parent)
            (delete computation
                    (composition-processes parent))))))

;;;; Running Processes

(defun run-process (process &optional (printp t))
  "* Syntax:
run-process process [printp] => process
* Arguments and Values:
- process---a {defclass amb::process}
- printp---a generalized boolean, default is true
* Description:
run-process runs the process by doing the following:
- When printp is true, print process.
- Compute the {defgeneric amb::evolutions} of process.
- If there are no evolutions, return process.
- Randomly select one of the evolutions and invoke it."
  (let ((*print-pprint-dispatch*
         *mobile-ambient-pprint-dispatch*))
    (flet ((output (n)
             (when printp
               (format *trace-output* "~&; ~A. ~:w" n process))))
      (output #\*)
      (cleanup process)
      (output 0)
      (do* ((n 1 (1+ n))
            (possibilities (evolutions process) (evolutions process)))
           ((endp possibilities) (values process (1- n)))
        (funcall (random-nth possibilities))
        (cleanup process)
        (output n)))))


