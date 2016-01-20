(use-modules (json))
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
  (let ((base-url (assoc #:glosbe base-urls))
        (url "url"))
    (display base-url)
    (newline)
    (display url)
    (newline)))

(meaning "sam")
