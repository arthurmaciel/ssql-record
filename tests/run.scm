(use test sql-record sql-null)

(define-sql-record doc (num date) name author doc_type)

(test-group "Record creation"
  (test "alist-><sql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . ,(sql-null)))
        (doc->alist (alist->doc '((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob")))))

  (test "list-><sql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . ,(sql-null)))
        (doc->alist (list->doc '(1 "5/11/2016" 2 "Bob"))))

  (test "Keywords parameters"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . ,(sql-null)))
        (make-doc num: 1 date: "5/11/2016" type: 2 author: "Bob")))

(test-group "Record accessors"
  (define d (alist->doc '((num . 1) (date . "5/11/2016") (name . 2) (author . "Bob"))))

  (test "<sql-record>->alist"
        `((num . 1) (date . "5/11/2016") (name . 2) (author . "Bob") (doc_type . ,(sql-null)))
        (doc->alist d))
  
  (test "<sql-record>-<accessor>"
        1
        (doc-num d))

  (test "<sql-record>-fields"
        '(num date name author doc_type)
        (doc-fields))

  (test "<sql-record>-select"
        '(select (columns num date) (from doc) (where (= id 3)))
        (doc-select '(columns num date) '((where (= id 3)))))

  (test "<sql-record>-rec-select"
        '(select (columns num date name author doc_type) (from doc) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-select d))

  (test "<sql-record>-select-all"
        '(select (columns num date name author doc_type) (from doc))
        (doc-select-all))

  (test "<sql-record>-update"
        '(update (table doc) (set (name 3) (author "Bob")) (where (= id 4)))
        (doc-update '((set (name 3) (author "Bob")) (where (= id 4)))))

  (test "<sql-record>-rec-update"
        `(update (table doc) (set (name 2) (author "Bob") (doc_type ,(sql-null))) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-update d))

  (test "<sql-record>-insert"
        '(insert (into doc) (3 "11/5/2016" 8 9 "Boo"))
        (doc-insert '((3 "11/5/2016" 8 9 "Boo"))))

  (test "<sql-record>-rec-insert"
        `(insert (into doc) (columns num date name author doc_type) (values #(1 "5/11/2016" 2 "Bob" ,(sql-null))))
        (doc-rec-insert d))

  (test "<sql-record>-delete"
        '(delete (from doc) (where (= id 3)))
        (doc-delete '((where (= id 3)))))

  (test "<sql-record>-rec-delete"
        '(delete (from doc) (where (= num 1) (= date "5/11/2016")))
        (doc-rec-delete d)))

(test-exit)
 
