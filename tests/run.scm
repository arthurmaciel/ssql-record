(use test ssql-record)

(define-ssql-record doc (num date) type author access_type)

(test-group "Record creation"
  (test "alist-><ssql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc->alist (alist->doc '((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob")))))

  (test "list-><ssql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc->alist (list->doc '(1 "5/11/2016" 2 "Bob"))))

  (test "Keywords parameters"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc->alist (make-doc num: 1 date: "5/11/2016" type: 2 author: "Bob"))))

(test-group "Record accessors"
  (define d (alist->doc '((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob"))))

  (test "<ssql-record>->alist"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc->alist d))
  
  (test "<ssql-record>-<field>"
        1
        (doc-num d))

  (test "<ssql-record>-fields"
        '(num date type author access_type)
        (doc-fields))

  (test "<ssql-record>-select"
        '(select (columns num date) (from doc) (where (= id 3)))
        (doc-select '(columns num date) '((where (= id 3)))))

  (test "<ssql-record>-rec-select"
        '(select (columns num date type author access_type) (from doc) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-select d))

  (test "<ssql-record>-select-all"
        '(select (columns num date type author access_type) (from doc))
        (doc-select-all))

  (test "<ssql-record>-update"
        '(update (table doc) (set (type 3) (author "Bob")) (where (= id 4)))
        (doc-update '((set (type 3) (author "Bob")) (where (= id 4)))))

  (test "<ssql-record>-rec-update"
        `(update (table doc) (set (type 2) (author "Bob") (access_type NULL)) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-update d))

  (test "<ssql-record>-insert"
        '(insert (into doc) (values #(3 "11/5/2016" 8 9 "Boo" NULL)))
        (doc-insert '((values #(3 "11/5/2016" 8 9 "Boo" NULL)))))

  (test "<ssql-record>-rec-insert"
        `(insert (into doc) (columns num date type author access_type) (values #(1 "5/11/2016" 2 "Bob" NULL)))
        (doc-rec-insert d))

  (test "<ssql-record>-delete"
        '(delete (from doc) (where (= id 3)))
        (doc-delete '((where (= id 3)))))

  (test "<ssql-record>-rec-delete"
        '(delete (from doc) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-delete d)))

(test-exit)
 
