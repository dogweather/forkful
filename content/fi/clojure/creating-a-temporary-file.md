---
title:                "Luo väliaikainen tiedosto"
html_title:           "Clojure: Luo väliaikainen tiedosto"
simple_title:         "Luo väliaikainen tiedosto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Sattuu joskus, että haluat luoda tilapäisiä tiedostoja ohjelmaasi varten (esim. väliaikaisia tietokantatiedostoja tai väliaikaisia kuvatiedostoja). Clojure tarjoaa helpon tavan tehdä tämä luomalla tilapäinen tiedosto ja varmistamalla sen poistamisen käytön jälkeen.

## Kuinka

```Clojure
(import '[java.io File])

;; Luo uusi tilapäinen tiedosto
(def temp-file (File/createTempFile "temp" ".txt"))

;; Kirjoita jotain tiedostoon
(spit temp-file "Hei maailma!")

;; Lue tiedostosta
(println (slurp temp-file))
;; Tulostaa: Hei maailma!

;; Poista tiedosto käytön jälkeen
(.delete temp-file)
```

## Syväsukellus

Clojuren `createTempFile` -funktio hyödyntää Javan `java.io.File` -luokkaa. Tämä luokka tarjoaa useita hyödyllisiä funktioita tiedostojen käsittelyyn, kuten tiedoston luomiseen, lukemiseen ja poistamiseen. Lisätietoja tästä luokasta löytyy Javan dokumentaatiosta.

## Katso myös

- Java `File` -luokan dokumentaatio: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html
- Clojuren `createTempFile` -funktion dokumentaatio: https://clojuredocs.org/clojure.java.io/create-temp-file