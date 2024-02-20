---
date: 2024-01-20 17:50:38.113165-07:00
description: "String-interpoloinnissa sijoitetaan muuttujia tai lausekkeita osaksi\
  \ merkkijonoja. Koodarit tekev\xE4t t\xE4t\xE4 dynaamisten tekstien luomiseen ja\
  \ koodin\u2026"
lastmod: 2024-02-19 22:05:15.112485
model: gpt-4-1106-preview
summary: "String-interpoloinnissa sijoitetaan muuttujia tai lausekkeita osaksi merkkijonoja.\
  \ Koodarit tekev\xE4t t\xE4t\xE4 dynaamisten tekstien luomiseen ja koodin\u2026"
title: Merkkijonon interpolointi
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
String-interpoloinnissa sijoitetaan muuttujia tai lausekkeita osaksi merkkijonoja. Koodarit tekevät tätä dynaamisten tekstien luomiseen ja koodin selkeyteen.

## How to: (Kuinka tehdään:)
```Clojure
;; Clojure ei sisällä oletuksena string-interpolaatiota, mutta voit käyttää `format` funktiota:
(defn greet [name]
  (format "Hello, %s!" name))

(greet "Mikko") ;; "Hello, Mikko!"

;; Tai voit käyttää `str` funktiota yhdistämään merkkijonoja:
(def name "Mikko")
(str "Hello, " name "!") ;; "Hello, Mikko!"
```

## Deep Dive (Sukellus syvyyksiin)
Clojure perustuu LISP:hen, joka on vanha ohjelmointikieli ilman sisäänrakennettua string-interpolaatiota. String-interpolaatio ei ole alkuperäisessä suunnitelmassa, mutta `format` tarjoaa vastaavan toiminnon käyttäen Java `String.format()` metodia. Vaihtoehtona `format`:lle on käyttää `str` to yhdistää merkkijonoja, tai voit ottaa käyttöön kirjastoja kuten `clojure.string` kanssa työskentelyyn. Yksi tärkeä asia Clojuressa on sen kyky käyttää Java-kirjastoja, mikä laajentaa sen toiminnallisuutta merkkijonojen käsittelyssä.

## See Also (Katso Myös)
- Clojure `format`: https://clojuredocs.org/clojure.core/format
- Clojure `str`: https://clojuredocs.org/clojure.core/str
- Clojure.org guide to String functions: https://clojure.org/guides/learn/functions#_string_functions
- Interop with Java: https://clojure.org/reference/java_interop
