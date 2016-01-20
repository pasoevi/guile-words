(use-modules (web client)
             (json)
             (ice-9 format)
             (ice-9 pretty-print))
(include "apis.scm")

(display
 (scm->json-string (json (array 1 2 3))))

(newline)

(define* (meaning phrase #:optional (source-lang #:en) (dest-lang #:en))
  "
   make calls to the glosbe API

  :param phrase: word for which meaning is to be found
  :param source-lang: Defaults to : ""
  :param dest_lang: Defaults to :"" For eg: "" for french
  :returns: returns a json object as str, False if invalid phrase
  "
  (let* ((base-url (assv-ref base-urls #:glosbe))
         (url (format #f base-url phrase )))
    (display url)
    (newline)
    (display (http-get url #:body #f #:streaming? #t))
    (newline)))

(meaning "sam")
