---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
JSON (JavaScript Object Notation) on kevyt datanvaihtoformaatti. Ohjelmoijat käyttävät sitä tiedon tallentamiseen ja siirtämiseen, koska se on helppolukuinen ihmisille ja koneille.

## Kuinka toimia:
```Clojure
; Ladataan Cheshire-kirjasto
(require '[cheshire.core :as json])

; Parsitaan JSON-merkkijono Clojure-mapiksi
(def jsonString "{\"name\":\"Juha\",\"age\":28}")
(def parsedData (json/parse-string jsonString true))

; Tulostetaan parsittu data
(println parsedData) ;; => {:name "Juha", :age 28}

; Muunnetaan Clojure-map JSON-merkkijonoksi
(def clojureData {:name "Marja" :age 35})
(def jsonString (json/generate-string clojureData))

; Tulostetaan JSON-merkkijono
(println jsonString) ;; => {"name":"Marja","age":35}
```

## Syväsukellus:
JSON sai alkunsa 2000-luvun alussa web-appsien kehittymisen myötä. Ennen JSONia käytettiin XML:tä, mutta JSON on noussut suosioon keveytensä ja yksinkertaisuutensa vuoksi. Clojuressa JSONin käsittelyyn löytyy kirjastoja, kuten `cheshire`, joka tarjoaa sekä synkronista että asynkronista parsintaa ja generointia. Se käyttää Jackson-kirjastoa tehokkuuden maksimoimiseen.

## Katso myös:
- Cheshire-kirjaston dokumentaatio: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- Clojure:n virallinen sivusto: [https://clojure.org/](https://clojure.org/)
- JSONin kotisivu ja spesifikaatiot: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
