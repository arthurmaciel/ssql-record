;; ssql-record - a simple egg to avoid ssql boilerplate code when prototyping
;;
;; Copyright (c) 2016, Arthur Maciel
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY ERESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module ssql-record
    (define-ssql-record)

  (import chicken scheme)
  (use data-structures typed-records srfi-1 sql-null)
  
  (define-syntax define-ssql-record
    (ir-macro-transformer
     (lambda (e i c)
       (let* ((record-name (cadr e))
              (ids (car (cddr e)))
              (field-names (flatten (cddr e)))
              (list->ssql-record (string->symbol (conc "list->" (i record-name))))
              (extend-record-name (lambda (ext) (string->symbol (conc (i record-name) ext))))            
              (fields (extend-record-name "-fields"))
              (alist (extend-record-name "-alist"))
              (where-list (extend-record-name "-where-list"))
              (select (extend-record-name "-select"))
              (rec-select (extend-record-name "-rec-select"))
              (select-all (extend-record-name "-select-all"))
              (update (extend-record-name "-update"))
              (rec-update (extend-record-name "-rec-update"))
              (insert (extend-record-name "-insert"))
              (rec-insert (extend-record-name "-rec-insert"))
              (delete (extend-record-name "-delete"))
              (rec-delete (extend-record-name "-rec-delete")))
         `(begin
            (defstruct ,record-name
              ,@(zip field-names (circular-list (quote (sql-null)))))

            (define ,list->ssql-record
              (lambda (lst)
                (,(string->symbol (conc "alist->" (i record-name)))
                 (map cons (quote ,field-names) lst))))
            
            (define ,fields (lambda () (quote ,field-names)))

            (define ,alist
              (lambda (rec)
                (,(string->symbol (conc (i record-name) "->alist")) rec)))

            (define ,where-list
              (lambda (rec)
                (map (lambda (id) `(= ,id ,(cdr (assoc id (,alist rec)))))
                     (quote ,ids))))          
            
            (define ,select
              (lambda (cols #!optional (ssql '()))
                `(select ,cols (from ,(quote ,record-name)) ,@ssql)))

            (define ,rec-select
              (lambda (rec #!optional (ssql '()))
                `(select (columns ,@(quote ,field-names))
                   (from ,(quote ,record-name))
                   (where ,@(,where-list rec))
                   ,@ssql)))
            
            (define ,select-all
              (lambda (#!optional (ssql '()))
                `(select (columns ,@(quote ,field-names))
                   (from ,(quote ,record-name)) ,@ssql)))
            
            (define ,update
              (lambda (ssql)
                `(update (table ,(quote ,record-name)) ,@ssql)))
            
            (define ,rec-update
              (lambda (rec)
                (let* ((alist-without-ids (filter (lambda (p) (not (member (car p) (quote ,ids)))) 
                                                  (,alist rec)))
                       (set-list (map (lambda (p) (list (car p) (cdr p))) ; proper list needed by ssql
                                      alist-without-ids)))
                  `(update (table ,(quote ,record-name)) 
                           (set ,@set-list)
                           (where ,@(,where-list rec))))))
            
            (define ,insert
              (lambda (ssql)
                `(insert (into ,(quote ,record-name)) ,@ssql)))
            
            (define ,rec-insert
              (lambda (rec)
                (let* ((alist (,alist rec))
                       (keys (map car alist))
                       (values (map cdr alist)))
                  `(insert (into ,(quote ,record-name)) (columns ,@keys) (values #(,@values))))))
            
            (define ,delete
              (lambda (#!optional (ssql '()))
                `(delete (from ,(quote ,record-name)) ,@ssql)))
            
            (define ,rec-delete
              (lambda (rec)
                `(delete (from ,(quote ,record-name)) 
                         (where ,@(,where-list rec))))))))))

  ) ;; End of module
