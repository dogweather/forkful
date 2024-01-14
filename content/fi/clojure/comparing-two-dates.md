---
title:                "Clojure: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Vertaamalla kahta päivämäärää voidaan tarkastella ajanjaksojen välillä tapahtuneita muutoksia, kuten päiviä, tunteja ja sekunteja.

## Miten tehdä
Vertailemalla kahta päivämäärää, voit käyttää Clojuren sisäänrakennettuja funktioita, kuten `compare` ja `days-between`. Katso alla olevat esimerkit ja lähtöarvot:

```Clojure
;; Vertaa kahta päivämäärää
(compare (java.util.Date. 2019 1 1) (java.util.Date. 2020 1 1))

;; Palauttaa positiivisen arvon, jos päivämäärä 1 on suurempi kuin päivämäärä 2
;; Palauttaa negatiivisen arvon, jos päivämäärä 1 on pienempi kuin päivämäärä 2
;; Palauttaa nollan, jos päivämäärät ovat samat

;; Laske päivien määrä kahden päivämäärän välillä
(days-between (java.util.Date. 2019 1 1) (java.util.Date. 2020 1 1))

;; Palauttaa 365 päivää
```

## Syvällinen sukellus
Verrattaessa kahta päivämäärää, on huomioitava aikavyöhykkeet. Voit käyttää `with-time-zone` -funktiota määrittääksesi aikavyöhykkeen, jolla haluat vertailun tapahtuvan. Lisäksi `java.util.Date` ei tue millisekuntien vertailua, joten käyttö `java.time.Instant` voi olla parempi vaihtoehto.

## Katso myös
- [Clojuren virallinen dokumentaatio päivämäärien vertailusta](https://clojuredocs.org/clojure.core/compare)
- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Blogiartikkeli päivämäärävertailusta Clojurella](https://stevenmiller888.github.io/mind-how-to-work-with-dates-in-clojure.html)