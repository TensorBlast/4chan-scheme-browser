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
	    (get-page2 pagenum (cdr ls))
	    )
	(error "WRONG FORMAT - CAN'T GET PAGE" ls)
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
  

(define-method (get-thread-endpoint thread)
  "http://a.4cdn.org/b/thread"
  #f
  read-json
  )

(define (get-thread thread)
  (cdar (get-thread-endpoint (conc thread ".json"))))


(define (get-image-ids threaddata)
  (if (null? threaddata) '()
      (let ((tim (get-inner-content 'tim (car threaddata))))
	(if (not (null? tim))
	    (cons tim
		  (get-image-ids (cdr threaddata)))
      (get-image-ids (cdr threaddata))))))
