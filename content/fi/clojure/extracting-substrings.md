---
title:                "Alirivien erottaminen"
html_title:           "Clojure: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Substringien hakeminen tarkoittaa osan merkkijonon erottamista. Tämä voi olla hyödyllistä esimerkiksi, kun tarvitaan tietyistä merkkijonoista tiettyjä osia tai kun halutaan tarkistaa, sisältääkö merkkijono tietyn alimerkkijonon. Ohjelmoijat voivat käyttää substringien hakemista monipuolisesti erilaisiin tarkoituksiin.

## Mitä pitäisi tehdä?

Alla on esimerkkejä eri tavoista hakea substringeja Clojure-kielellä.

```Clojure
;; Hakukomennolla voi hakea tietyn määrän merkkejä alusta lähtien:
(subs "Tämä on teksti" 0 4)
;; Tulostaa "Tämä"

;; Myös merkkien lukumäärä lopusta lähtien on mahdollista määrittää:
(subs "Tämä on teksti" -5)
;; Tulostaa "teksti"

;; Tiettyyn merkkijonoon voi myös viitata hakemalla sen indeksin avulla:
(get "Tämä on teksti" 3)
;; Tulostaa \ä
```

## Syvemmälle:

Substringien hakeminen on ollut käytössä ohjelmoinnissa jo pitkään. Useilla kielillä, kuten Java ja Python, on omat metodinsa substringien hakemiseen. Clojuressa substr-hakukomentoa käytetään yleisesti, mutta myös muita vaihtoehtoja, kuten reaioita ja regex:ejä, voidaan hyödyntää.

## Katso myös:

Lisätietoja substringien hakemisesta Clojurella löytyy Clojuren viralliselta verkkosivulta: https://clojure.org/guides/strings

Lisää tietoa regex:istä löytyy täältä: https://clojure.org/guides/regexes

Java-kieleen perustuvan substr-komennon käyttöohjeita löytyy täältä: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html