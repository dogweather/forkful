---
date: 2024-01-20 17:42:00.366256-07:00
description: "Mik\xE4 ja miksi? Ohjelmoinnissa merkkien poistaminen kuvioon sopivasti\
  \ on datan siistimist\xE4. Tehd\xE4\xE4n n\xE4timp\xE4\xE4, helpommin k\xE4sitelt\xE4\
  v\xE4\xE4."
lastmod: 2024-02-19 22:05:15.110687
model: gpt-4-1106-preview
summary: "Mik\xE4 ja miksi? Ohjelmoinnissa merkkien poistaminen kuvioon sopivasti\
  \ on datan siistimist\xE4. Tehd\xE4\xE4n n\xE4timp\xE4\xE4, helpommin k\xE4sitelt\xE4\
  v\xE4\xE4."
title: Merkkien poistaminen hakemalla osumia kaavaan
---

{{< edit_this_page >}}

## What & Why?
Mikä ja miksi? Ohjelmoinnissa merkkien poistaminen kuvioon sopivasti on datan siistimistä. Tehdään nätimpää, helpommin käsiteltävää.

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
