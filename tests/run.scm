(use test sql-record sql-null)

(define-sql-record doc (num date) name author doc_type)

(test-group "Record creation"
  (test "alist-><sql-record>"
        `((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur") (doc_type . ,(sql-null)))
        (begin
          (define d (alist->doc '((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur"))))
          (doc->alist d)))

  (test "list-><sql-record>"
        `((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur") (doc_type . ,(sql-null)))
        (begin
          (define d (alist->doc '((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur"))))
          (doc->alist d))))

(test-group "Record 1"
 (test "<sql-record>->alist"
       `((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur") (doc_type . ,(sql-null)))
       (doc->alist d))
 
 (test "<sql-record>-<accessor>"
       1
       (doc-num d))

 (test "<sql-record>-fields"
       '(num date name author doc_type)
       (doc-fields))

 (test "<sql-record>-select"
       '(select (columns *) (from doc) (where (= id 3)))
       (doc-select '(columns *) '(where (= id 3))))

 (test "<sql-record>-select/id"
       '(select (columns num date name author doc_type) (from doc) (where (= num 1) (= date "5/11/2016")))
       (doc-select/id d))

 (test "<sql-record>-select/id"
       '(select (columns num date name author doc_type) (from doc))
       (doc-select-all))

 (test "<sql-record>-update"
       '(update (table doc) where (= id 4))
       (doc-update '(where (= id 4))))

 (test "<sql-record>-update/id"
       `(update (table doc) (set (name 2) (author "Arthur") (doc_type ,(sql-null))) (where (= num 1) (= date "5/11/2016")))
       (doc-update/id d))

 (test "<sql-record>-insert"
       '(insert into doc (3 "11/5/2016" 8 9 "Boo"))
       (doc-insert '((3 "11/5/2016" 8 9 "Boo"))))

 (test "<sql-record>-insert/id"
       `(insert into doc (columns num date name author doc_type) (values #(1 "5/11/2016" 2 "Arthur" ,(sql-null))))
       (doc-insert-all d))

 (test "<sql-record>-delete"
       '(delete (from doc) where (= id 3))
       (doc-delete '(where (= id 3))))
 (test "<sql-record>-delete/id"
       '(delete (from doc) (where (= num 1) (= date "5/11/2016")))
       (doc-delete/id d)))


(define-sql-record people (id) name dob gender)

(define d (alist->people '((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur"))))

(test-group "Record 1"
  (test "<sql-record>->alist"
        `((num . 1) (date . "5/11/2016") (name . 2) (author . "Arthur") (people_type . ,(sql-null)))
        (people->alist d))
  
  (test "<sql-record>-<accessor>"
        1
        (people-num d))

  (test "<sql-record>-fields"
        '(num date name author people_type)
        (people-fields))

  (test "<sql-record>-select"
        '(select (columns *) (from people) (where (= id 3)))
        (people-select '(columns *) '(where (= id 3))))

  (test "<sql-record>-select/id"
        '(select (columns num date name author people_type) (from people) (where (= num 1) (= date "5/11/2016")))
        (people-select/id d))

  (test "<sql-record>-select/id"
        '(select (columns num date name author people_type) (from people))
        (people-select-all))

  (test "<sql-record>-update"
        '(update (table people) where (= id 4))
        (people-update '(where (= id 4))))

  (test "<sql-record>-update/id"
        `(update (table people) (set (name 2) (author "Arthur") (people_type ,(sql-null))) (where (= num 1) (= date "5/11/2016")))
        (people-update/id d))

  (test "<sql-record>-insert"
        '(insert into people (3 "11/5/2016" 8 9 "Boo"))
        (people-insert '((3 "11/5/2016" 8 9 "Boo"))))

  (test "<sql-record>-insert/id"
        `(insert into people (columns num date name author people_type) (values #(1 "5/11/2016" 2 "Arthur" ,(sql-null))))
        (people-insert-all d))

  (test "<sql-record>-delete"
        '(delete (from people) where (= id 3))
        (people-delete '(where (= id 3))))

  (test "<sql-record>-delete/id"
        '(delete (from people) (where (= num 1) (= date "5/11/2016")))
        (people-delete/id d)))


(test-exit)
 
