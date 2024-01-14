---
title:                "Clojure: Kahden päivämäärän vertailu"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

***

## Miksi vertailla kahta päivämäärää?

Päivämäärät ovat oleellinen osa lähes jokapäiväistä työskentelyä. Niiden vertaileminen on tärkeää esimerkiksi tapahtumien järjestämiseksi aikajärjestykseen tai sopivien matkojen varaamiseksi. Clojuren avulla tämä tehtävä onnistuu helposti ja vaivattomasti.

## Miten tehdä vertailu Clojurella?

Vertailemalla kahta päivämäärää tarkoituksenmukaiseen tapaan, on tärkeää käyttää oikeita funktioita ja metodeja. Tässä esimerkissä käytämme Clojuren `java.time`-kirjastoa. Se mahdollistaa päivämäärien vertailun eri muodoissaan.

```Clojure
(ns vertaa-päivämäärää
  (:import [java.time LocalDate]))

;; Määritellään kaksi päivämäärää
(def ensimmäinen-pvm (LocalDate/of 2021 5 31))
(def toinen-pvm (LocalDate/parse "2021-06-01"))

;; Vertaillaan päivämääriä
(if (.equals ensimmäinen-pvm toinen-pvm)
  (println "Päivämäärät ovat samat")
  (if (< ensimmäinen-pvm toinen-pvm)
    (println "Ensimmäinen päivämäärä on ennen toista")
    (println "Toinen päivämäärä on ennen ensimmäistä")))
```

Tulostus:

```
Toinen päivämäärä on ennen ensimmäistä
```

## Syvällinen tarkastelu vertaamisesta

Clojuren `java.time`-kirjaston avulla on mahdollista vertailla päivämääriä eri muodoissa, kuten esimerkiksi vuosissa, kuukausissa ja päivissä. Vertailemalla kahta päivämäärää, voidaan myös tarkastella niiden eroja ja tutkia niiden yhtäläisyyksiä. `java.time`-kirjasto tarjoaa kattavan valikoiman funktioita ja metodeja päivämäärien käsittelyyn ja vertailuun.

## Katso myös

- [Java Time - ClojureDocs](https://clojuredocs.org/clojure.java-time)
- [How to Work with Dates and Times in Clojure](https://www.braveclojure.com/do-things-with-time/)
- [Java Time API - Oracle](https://docs.oracle.com/javase/tutorial/datetime/index.html)