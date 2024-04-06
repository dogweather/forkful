---
date: 2024-01-20 17:42:00.366256-07:00
description: "How to: Syvensin\xE4. Ennen satoja ohjelmointikieli\xE4, datan siivous\
  \ oli manuaalista. Clojure, noin vuodesta 2007, tarjoaa modernit ty\xF6kalut kuten\
  \ regex\u2026"
lastmod: '2024-04-05T21:53:57.725723-06:00'
model: gpt-4-1106-preview
summary: ''
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to:
```Clojure
;; Esimerkki: Poista kaikki numerot stringistä

(defn poista-numerot [teksti]
  (clojure.string/replace teksti #"\d+" ""))

;; Käyttö:
(poista-numerot "abc123def456")
;; Output: "abcdef"
```

```Clojure
;; Esimerkki: Poista erikoismerkit
(defn poista-erikoismerkit [teksti]
  (clojure.string/replace teksti #"[^\w\s]" ""))

;; Käyttö:
(poista-erikoismerkit "Hello, World!")
;; Output: "Hello World"
```

## Deep Dive
Syvensinä. Ennen satoja ohjelmointikieliä, datan siivous oli manuaalista. Clojure, noin vuodesta 2007, tarjoaa modernit työkalut kuten regex (säännölliset lausekkeet) ja string-manipulaatio funktiot. Alternatiiveja? Muut kielet, funktiot. Mutta Clojuren funktionaalinen luonne tekee patterneihin sopivien merkkien poistosta yksinkertaista.

## See Also
Related Links:

- Clojure doc: [https://clojuredocs.org/clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- Regex guide: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
- String manipulation: [https://clojure.org/guides/weird_characters](https://clojure.org/guides/weird_characters)
