;;; (guile-words) -- a vocabulary library to find the meaning, synonyms,
;;; antonyms and more for a given word.
;;;
;;; Copyright (C) 2016 Sergi Pasoev
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301 USA

(define version "0.01")

(define-module (words)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (gnutls)
  #:export (meaning
            synonym
            antonym
            similar
            related
            hyphenation
            pronunciation))

(define base-urls
  '((#:glosbe . "https://glosbe.com/gapi/translate?from=~a&dest=~a&format=json&pretty=true&phrase=~a")
    (#:wordnik . "http://api.wordnik.com/v4/word.json/~a/~a?api_key=9a93b2e159528c8d7d1050840b7065a6a64569e8e4b8e6e73")
    (#:urbandict . "http://api.urbandictionary.com/v0/~a?term=~a")
    (#:bighugelabs . "http://words.bighugelabs.com/api/2/6a7727e0e5cb684b8de04f34faa83665/~a/json")))

(define langs
  '((#:en . "en")))

(define actions
  '((#:meaning . "define")
    (#:synonym . "syn")
    (#:antonym . "ant")
    (#:related . "rel")
    (#:similar . "sim")
    (#:usage-examples . "usage-examples")
    (#:hyphenation . "hyphenation")
    (#:pronunciation . "pronunciations")
    (#:define . "definitions")))

(define (call-service url)
  (utf8->string
   (read-response-body
    (http-get url #:streaming? #t))))

(define* (lookup provider word #:optional (source-lang #:en) (dest-lang #:en) (action "error"))
  (let* ((base-url (assv-ref base-urls provider))
         (from (assv-ref langs source-lang))
         (to (assv-ref langs dest-lang))
         (actn (assv-ref actions action))
         (url (cond
               ((eq? provider #:wordnik) (format #f base-url word actn))
               ((eq? provider #:glosbe) (format #f base-url from to word))
               ((eq? provider #:urbandict) (format #f base-url #:define word))
               ((eq? provider #:bighugelabs) (format #f base-url word)))))
    (call-service url)))

(define* (glosbe word #:optional (source-lang #:en) (dest-lang #:en))
  (lookup #:glosbe word source-lang dest-lang))

(define* (wordnik word action #:optional (source-lang #:en) (dest-lang #:en))
  (lookup #:wordnik word source-lang dest-lang action))

(define (urbandict word)
  (lookup #:urbandict word))

(define (bighugelabs word)
  (lookup #:bighugelabs word))

(define* (meaning phrase #:optional (source-lang #:en) (dest-lang #:en))
  "
   make calls to the glosbe API

  :param phrase: word for which meaning is to be found
  :param source-lang: Defaults to : ""
  :param dest-lang: Defaults to :"" For eg: "" for french
  :returns: returns a json object as str, False if invalid phrase
  "
  (wordnik phrase #:define))

(define (parse-bighuge word action)
  (let ((result (json-string->scm (bighugelabs word)))
        (lst (list action))
        (act (assv-ref actions action)))
    (hash-for-each
     (lambda (key value)
       (hash-for-each
        (lambda (k v)
          (when (string=? k act)
            (append! lst v)))
        value))
     result)
    lst))

(define (synonym word)
  (parse-bighuge word #:synonym))

(define (related word)
  (parse-bighuge word #:related))

(define (similar word)
  (parse-bighuge word #:similar))

(define (antonym word)
  (parse-bighuge word #:antonym))

(define (hyphenation word)
  (wordnik word #:hyphenation))

;; TODO: fix the unicode problem
(define (pronunciation word)
  (wordnik word #:pronunciation))

(define usage-examples "Not implemented")
(define part-of-speech "Not implemented")

(define* (process action phrase #:optional (source-lang #:en) (dest-lang #:en))
  (action phrase source-lang dest-lang))
