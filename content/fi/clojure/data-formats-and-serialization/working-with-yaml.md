---
title:                "Työskentely YAML:n kanssa"
date:                  2024-02-03T19:24:56.378953-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML, rekursiivinen lyhenne sanoista "YAML Ain't Markup Language", on ihmisen luettavissa oleva datan sarjallistamisformaatti, jota käytetään määritystiedostoissa ja datan vaihdossa eri tietorakenteita käyttävien kielten välillä. Ohjelmoijat käyttävät YAMLia sen yksinkertaisuuden ja luettavuuden vuoksi, mikä tekee siitä ihanteellisen valinnan sovellusten konfigurointiin ja datan vaihtoon polyglot-ohjelmointiympäristöissä.

## Kuinka:

Clojure ei sisällä sisäänrakennettua tukea YAMLille, mutta voit käyttää kolmannen osapuolen kirjastoja, kuten `clj-yaml`, YAML-datan jäsentämiseen ja luomiseen. Ensiksi, lisää kirjasto projektisi riippuvuuksiin:

```clojure
;; Lisää tämä projektisi project.clj riippuvuuksiin
[clj-yaml "0.7.0"]
```

Tässä on miten voit käyttää `clj-yaml`ia YAMLin jäsentämiseen ja Clojure map:ien muuntamiseen YAMLiksi.

### YAMLin jäsentäminen:

```clojure
(require '[clj-yaml.core :as yaml])

;; YAML-merkkijonon jäsentäminen
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Tuloste:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### YAMLin luominen Clojuresta:

```clojure
(require '[clj-yaml.core :as yaml])

;; Clojure map:in muuntaminen YAML-merkkijonoksi
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Tuloste:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

Nämä yksinkertaiset toiminnot `clj-yaml`in avulla voidaan integroida Clojure-sovelluksiin käsittelemään määritystiedostoja tai helpottamaan datan vaihtoa muiden palveluiden tai komponenttien kanssa, jotka käyttävät YAMLia.
