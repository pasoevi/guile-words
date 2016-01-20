(use-modules (web client))

(define base-urls
  '((#:glosbe . "https://glosbe.com/gapi/translate?from=en&dest=en&format=json&pretty=true&phrase=~a")
    (#:wordnik . "http://api.wordnik.com/v4/word.json/~a/~a?api_key=1e940957819058fe3ec7c59d43c09504b400110db7faa0509")
    ;; (#:urbandict . "http://api.urbandictionary.com/v0/{action}?term={word}")
    ;;(#:bighugelabs . "http://words.bighugelabs.com/api/2/eb4e57bb2c34032da68dfeb3a0578b68/~a/json")
    ))
