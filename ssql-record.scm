;; ssql-record - a simple egg to avoid SSQL boilerplate code when prototyping
;;
;; Copyright (c) 2017, Arthur Maciel
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
(use data-structures typed-records srfi-1)

(define-for-syntax translate
  (lambda (str)
    (string->symbol (string-translate* (->string str) '(("-" . "_"))))))

(define-syntax define-ssql-record
  (ir-macro-transformer
   (lambda (e i c)
     (let* ((record-name (strip-syntax (cadr e)))
            (translated-record-name (i (translate record-name)))
            (ids (strip-syntax (car (cddr e))))
            (translated-ids (map translate ids))
            (field-names (strip-syntax (flatten (cddr e))))
            (translated-field-names (map translate field-names))
            (list->ssql-record (i (string->symbol (conc "list->" record-name))))
            (extend-record-name (lambda (ext) (string->symbol (conc record-name ext))))
            (fields (extend-record-name "-fields"))
            (alist (extend-record-name "->alist"))
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
            ,@(zip (i field-names) (circular-list (quote 'NULL))))

          (define ,list->ssql-record
            (lambda (lst)
              (,(string->symbol (i (conc "alist->" record-name)))
               (map cons (quote ,(i field-names)) lst))))
          
          (define ,fields (lambda () (quote ,(i field-names))))

          (define ,alist
            (lambda (rec)
              (list . ,(map (lambda (field)
                              `(cons (quote ,(translate field))
                                     (,(i (string->symbol (conc record-name "-" field))) rec)))
                            field-names))))

          (define ,where-list
            (lambda (rec)
              (map (lambda (id)
                     `(= ,(translate id) ,(cdr (assoc id (,alist rec)))))
                   (quote ,(i ids)))))          
          
          (define ,select
            (lambda (cols #!optional (ssql '()))
              `(select ,cols
                 (from ,(quote ,translated-record-name))
                 ,@ssql)))

          (define ,rec-select
            (lambda (rec #!optional (ssql '()))
              `(select (columns ,@(quote ,translated-field-names))
                 (from ,(quote ,translated-record-name))
                 (where ,@(,where-list rec))
                 ,@ssql)))
          
          (define ,select-all
            (lambda (#!optional (ssql '()))
              `(select (columns ,@(quote ,translated-field-names))
                 (from ,(quote ,translated-record-name))
                 ,@ssql)))
          
          (define ,update
            (lambda (ssql)
              `(update (table ,(quote ,translated-record-name))
                       ,@ssql)))
          
          (define ,rec-update
            (lambda (rec)
              (let* ((alist-without-ids (filter (lambda (p)
                                                  (not (member (car p) (quote ,(i ids)))))
                                                (,alist rec)))
                     (set-list (map (lambda (p)
                                      (list (translate (car p)) (cdr p))) ; proper list needed by ssql
                                    alist-without-ids)))
                `(update (table ,(quote ,translated-record-name)) 
                         (set ,@set-list)
                         (where ,@(,where-list rec))))))
          
          (define ,insert
            (lambda (ssql)
              `(insert (into ,(quote ,translated-record-name)) ,@ssql)))
          
          (define ,rec-insert
            (lambda (rec)
              (let* ((alist (,alist rec))
                     (ids-values (map cdr 
                                      (filter (lambda (p) (member (car p) (quote ,(i ids))))
                                              alist)))
                     (alist* (if (any (lambda (id) (eq? 'NULL id)) ids-values)
                                 (filter (lambda (p) (not (member (car p) (quote ,(i ids)))))
                                         alist)
                                 alist))
                     (keys (map car alist*))
                     (values (map cdr alist*)))
                `(insert (into ,(quote ,translated-record-name))
                         (columns ,@keys)
                         (values #(,@values))))))
          
          (define ,delete
            (lambda (#!optional (ssql '()))
              `(delete (from ,(quote ,translated-record-name))
                       ,@ssql)))
          
          (define ,rec-delete
            (lambda (rec)
              `(delete (from ,(quote ,translated-record-name)) 
                       (where ,@(,where-list rec))))))))))

) ;; End of module
