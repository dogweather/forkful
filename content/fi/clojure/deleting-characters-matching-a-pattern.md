---
title:                "Clojure: Mallia vastaavien merkkien poisto"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Yksi syy voisi olla, että haluat puhdistaa tekstisi ja poistaa ylimääräiset tai turhat merkit.

## Kuinka

Tekeminen tämä on yksinkertaista käyttämällä Clojuren `clojure.string/replace`-toimintoa. Koodin kirjoittaminen tällä tavalla vaatii muutaman askeleen:

```
Clojure (require '[clojure.string :as string])

(def teksti "Tämä on esimerkki, jossa haluan poistaa kaikki numerot 12345.")
(def kaava #"[0-9]+")

(defn poista-merkit [teksti kaava]
  (string/replace teksti kaava ""))

(poista-merkit teksti kaava)
```

Tämä koodiesimerkki käyttää `string/replace`-toimintoa poistaa kaikki numerot, jotka vastaavat kaavaa `#"[0-9]+"`. Lopputuloksena tekstisi on "Tämä on esimerkki, jossa haluan poistaa kaikki numerot."

## Syventyvä tarkastelu

Voit käyttää myös säännöllisiä lausekkeita poistamaan merkkejä, jotka vastaavat tiettyä kaavaa. Esimerkiksi, jos haluat poistaa kaikki välilyönnit, voit käyttää `#"\s+"`-kaavaa. Tämä korvaa kaikki välilyönnit tekstillä "":

```
(poista-merkit "Tämä on esimerkki, jossa haluan poistaa kaikki välilyönnit." #"\s+")
```

Lopputulos on teksti "Tämäonesimerkki,jossahaluanpoistaakaikkivälilyönnit."

## Katso myös

- [Clojure-kielen virallinen verkkosivusto](https://clojure.org/)
- [Clojure String-kirjaston dokumentaatio](https://clojure.github.io/clojure/clojure.string-api.html)
- [Säännölliset lausekkeet Clojuressa](https://clojuredocs.org/clojure.core/re-seq)