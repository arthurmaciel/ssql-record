(use test ssql-record)

(define-ssql-record doc-table (num date) type author access-type)

(test-group "Record creation"
  (test "alist-><ssql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc-table->alist (alist->doc-table '((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob")))))

  (test "list-><ssql-record>"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc-table->alist (list->doc-table '(1 "5/11/2016" 2 "Bob"))))

  (test "Keywords parameters"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc-table->alist (make-doc-table num: 1 date: "5/11/2016" type: 2 author: "Bob"))))

(test-group "Record accessors"
  (define d (alist->doc-table '((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob"))))

  (test "<ssql-record>->alist"
        `((num . 1) (date . "5/11/2016") (type . 2) (author . "Bob") (access_type . NULL))
        (doc-table->alist d))
  
  (test "<ssql-record>-<field>"
        1
        (doc-table-num d))

  (test "<ssql-record>-fields"
        '(num date type author access-type)
        (doc-table-fields))
  
  (test "<ssql-record>-select"
        '(select (columns num date) (from doc_table) (where (= id 3)))
        (doc-table-select '(columns num date) '((where (= id 3)))))

  (test "<ssql-record>-rec-select"
        '(select (columns num date type author access_type) (from doc_table) (where (= num 1) (= date "5/11/2016")))
        (doc-table-rec-select d))

  (test "<ssql-record>-select-all"
        '(select (columns num date type author access_type) (from doc_table))
        (doc-table-select-all))

  (test "<ssql-record>-update"
        '(update (table doc_table) (set (type 3) (author "Bob")) (where (= id 4)))
        (doc-table-update '((set (type 3) (author "Bob")) (where (= id 4)))))

  (test "<ssql-record>-rec-update"
        `(update (table doc_table) (set (type 2) (author "Bob") (access_type NULL)) (where (= num 1) (= date "5/11/2016")))
        (doc-table-rec-update d))

  (test "<ssql-record>-insert"
        '(insert (into doc_table) (values #(3 "11/5/2016" 8 9 "Boo" NULL)))
        (doc-table-insert '((values #(3 "11/5/2016" 8 9 "Boo" NULL)))))

  (test "<ssql-record>-rec-insert"
        `(insert (into doc_table) (columns num date type author access_type) (values #(1 "5/11/2016" 2 "Bob" NULL)))
        (doc-table-rec-insert d))

  (test "<ssql-record>-delete"
        '(delete (from doc_table) (where (= id 3)))
        (doc-table-delete '((where (= id 3)))))

  (test "<ssql-record>-rec-delete"
        '(delete (from doc_table) (where (= num 1) (= date "5/11/2016")))
        (doc-table-rec-delete d)))

(test-group "Translation"
  (test "Parameterizing 'translate'"
        `(update (table test__table)
                 (set (test__name "Bob") (test__address "Bob St."))
                 (where (= id  1)))
        (begin
          (define-for-syntax translate ;; need to be placed *before* define-ssql-record
            (lambda (str)
              (string->symbol (string-translate* (->string str) '(("-" . "__"))))))
          (define-ssql-record test-table (id) test-name test-address)
          (test-table-rec-update (alist->test-table '((id . 1) (test-name . "Bob") (test-address . "Bob St.")))))))

(test-exit)

