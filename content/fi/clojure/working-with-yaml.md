---
title:                "Työskentely yaml:n kanssa"
html_title:           "Clojure: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
YAML on tiedostoformaatti, jota käytetään tietojen tallentamiseen ja siirtämiseen ohjelmistokehityksessä. Se on yksinkertainen ja helposti luettava formaatti, joka koostuu loogisista hierarkkisista rakenteista. Ohjelmoijat käyttävät YAML:ia säilyttääkseen tärkeitä tietoja, kuten konfiguraatiota ja tietokantayhteyksiä, jotta niitä voidaan käyttää ohjelmassa helposti.

## Kuinka:
```Clojure
;; Lisää Clojure-yaml riippuvuus projektin pom.xml-tiedostoon
[org.clojure/clojure-yaml "1.2.0"]

;; Tuodaan clojure-yaml kirjasto
(require '[clojure-yaml.core :as yaml])

;; Luodaan YAML-tiedosto
(def yaml-data "---\nname: Jane\nage: 25")

;; Muutetaan YAML-muoto Clojure-muotoon
(def clojure-data (yaml/read-string yaml-data))

;; Tulostetaan Clojure-data
(:name clojure-data) ;; Jane
(:age clojure-data) ;; 25
```

## Syväsukellus:
YAML kehitettiin vuonna 2001, ja siitä on tullut suosittu tiedostoformaatti ohjelmistokehittäjien keskuudessa sen yksinkertaisuuden ja selkeyden vuoksi. Toisin kuin muut tiedostoformaatit, kuten XML ja JSON, YAML ei vaadi erikoismerkkejä tai tagiksiä, mikä tekee siitä helpommin luettavan ihmisille. Lisäksi YAML tukee kommentteja, mikä tekee siitä hyödyllisen monimutkaisemmissa asetustiedostoissa.

## Katso myös:
- [Clojure-yaml-kirjaston virallinen dokumentaatio](https://github.com/ninjudd/clojure-yaml)
- [YAML-spesifikaatio](https://yaml.org/)
- [Clojure-yaml-kirjaston lähdekoodi GitHubissa](https://github.com/ninjudd/clojure-yaml)