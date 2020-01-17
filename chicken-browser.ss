(import rest-bind http-client (chicken io) medea (chicken string))

(define-method (threadlist board "threads.json")
  "http://a.4cdn.org"
  #f
  read-json
  )

(define array-as-list-parser
  (cons 'array (lambda (x) x)))

(json-parsers (cons array-as-list-parser (json-parsers)))

(define (threads)
  json->string (threadlist "g"))


(define (get-page pagenum ls)
  (let ((current-page (caar ls)))
    (if (eq? (car current-page) 'page)
	(if (eqv? (cdr current-page) pagenum)
	    (cdadar ls)
	    (get-page pagenum (cdr ls))
	    )
	(error "WRONG FORMAT - CAN'T GET PAGE" ls)
	)
    )
  )

(define (display-threadlist data board)

  (let disp ((dat data) (i 1))
      (if (null? dat) (display " * END * ")
	(let* (
	       (id (get-inner-content 'no (car dat)))
	       (replies (get-inner-content 'replies (car dat)))
	     (thread-data (get-thread board (number->string id)))
	     (thread-subject (get-inner-content 'sub (car thread-data))))
	  (begin
	    (display "No.: ")
	    (display i)
	    (newline)
	  (display "Thread ID: ")
	  (display id)
	  (newline)
	  (display "Sub: ")
	  (if (not (null? thread-subject))
	      (display thread-subject)
	      (display "NA"))
	  (newline)
	  (display "Replies: ")
	  (display replies)
	  (newline)
	  (display "------------------------")
	  (newline)
	  (disp (cdr dat) (+ i 1))
	  ))
	)
      )
  )

(define (thread-ids data)
  (if (null? data) '()
      (let ((currentno (get-inner-content 'no (car data))))
	(if (null? currentno)
	    (thread-ids (cdr data))
	    (cons currentno (thread-ids (cdr data)))))))

(define (get-inner-content name ls)
  (if (null? ls) '()
  (let ((current (caar ls)))
    (if (eqv? current name)
	(cdar ls)
	(get-inner-content name (cdr ls))))))

(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list))
	    (map proc (cdr list)))
      )
  )
  

(define-method (get-thread-endpoint board "thread" thread)
  "http://a.4cdn.org"
  #f
  read-json
  )

(define (get-thread board thread)
  (cdar (get-thread-endpoint board (conc thread ".json"))))


(define (get-image-ids threaddata)
  (if (null? threaddata) '()
      (let ((tim (get-inner-content 'tim (car threaddata))))
	(if (not (null? tim))
	    (cons tim
		  (get-image-ids (cdr threaddata)))
      (get-image-ids (cdr threaddata))))))


(define board "g")
(define current-page 1)

(define (boardselector)
  (newline)
  (display "Select board to browse:")
  (newline)
  (display "1. /g/")
  (newline)
  (display "2. /b/")
  (newline)
  (let ((input (string->number (read-line))))
    (if (and (number? input) (> input 0) (<= input 2))
	(cond
	 ((= input 1) (set! board "g"))
	 ((= input 2) (set! board "b"))
	 )
	(error "Incorrect input entered" input)
	)
    )
  (display "Board set to - ")
  (display board)
  (newline)
  )

(define (main)
  (let loop ()
    (display "4chan Browser")
    (newline)
    (boardselector)
    (newline)
    (let f ((thread-data (get-page current-page (threadlist board))))
      (display (conc "PAGE : " current-page))
      (newline)
      (display-threadlist thread-data board)
      (if (> current-page 1)
	  (display "1. Previous Page | 2. Next Page")
	  (display "1. Next Page"))
      (display newline)
      (let ((input (string->number (read-line))))
	(cond ((and (= input 1) (> current-page 1)) (set! current-page (- current-page 1)))
	      ((= input 2) (set! current-page (+ current-page 1)))
	      (else (set! current-page (+ current-page 1))))
	(if (< current-page 1) (set! current-page 1)
	    )
      (f (get-page current-page (threadlist board)))
      )
    )))
