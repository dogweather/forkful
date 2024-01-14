---
title:                "Clojure: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tilanteissa, kuten tekstieditoriaalin kirjoittaessa tai koodia muokatessa, on tarpeen korvata tiettyä merkkijonoa tai ottaa käyttöön tiettyä muutosta koko dokumenttiin. Tämä voi säästää aikaa ja vaivaa manuaalisen muutoksen tekemisessä ja auttaa varmistamaan, että dokumentti on yhdenmukainen ja virheetön.

## Miten

Clojurella on käytännöllisiä työkaluja tekstien etsimiseen ja korvaamiseen. Alla on esimerkkejä, jotka näyttävät, kuinka voit käyttää Clojuren `replace` ja `replace-all` -funktioita korvaamaan tekstiä. Tämä käyttäjä syöttää tekstin "Tervetuloa" ja korvaa sen tekstillä "Hei":

```Clojure
(replace "Tervetuloa" "Hei" "Tervetuloa uudelleen!") => "Hei uudelleen!"
(replace-all "Tervetuloa" "Hei" "Tervetuloa uudelleen!") => "Hei uudelleen Hei!"

```

`replace` -funktio korvaa vain ensimmäisen esiintymän tekstissä, kun taas `replace-all` -funktio korvaa kaikki esiintymät. Voit myös käyttää Clojuren `re-find` -funktiota etsimään ja korvaamaan säännöllisten lausekkeiden avulla:

```Clojure
(re-find #"tarkista\d\d\d" "Tarkista123 ja Tarkista456") => "tarkista123"
(replace #"tarkista\d\d\d" "muutettu" "Tarkista123 ja Tarkista456") => "muutettu ja muutettu"

```

## Syvempi sukellus

Clojurella on myös muita työkaluja tekstien etsimiseen ja korvaamiseen, kuten `re-seq`-funktio, joka etsii kaikki esiintymät ja palauttaa ne listana. Voit myös käyttää `re-pattern` -funktiota muuntaaksesi merkkijonon säännölliseksi lausekkeeksi. Lisätietoja näistä ja muista tekstinkäsittelymahdollisuuksista löytyy Clojuren dokumentaatiosta.

## Katso myös

- [Clojuren virallinen dokumentaatio](https://clojure.org/reference/text_processing)
- [Clojure-järjestelmän etsiminen ja korvaaminen - esimerkkejä](https://www.baeldung.com/clojure-string-replace)
- [Yksityiskohtaiset ohjeet säännöllisten lausekkeiden käytöstä Clojurella](https://github.com/jafingerhut/thalia/blob/master/userguide.adoc#section-25-string-processing-regular-expressions)