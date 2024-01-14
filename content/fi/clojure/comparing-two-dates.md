---
title:    "Clojure: Kahden päivämäärän vertailu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä ja sovelluksissa on tarpeen vertailla kahta päivämäärää, kuten esimerkiksi tapahtumien ajoitusta ja aikarajoja varten.

## Miten tehdä

Vertailemalla kahta päivämäärää Clojure-ohjelmointikielellä voidaan käyttää sisäänrakennettua funktiota `compare` ja siihen liittyviä funktioita, kuten `local-date` ja `after?`. Katso alla olevaa esimerkkiä:

```
Clojure (compare (local-date 2019 03 18) (local-date 2019 03 20))
```

Tämä palauttaa `-1`, mikä tarkoittaa, että ensimmäinen päivämäärä on ennen toista päivämäärää. Jos haluamme tarkistaa, onko ensimmäinen päivämäärä jälkeen toista päivämäärää, voimme käyttää `after?` -funktiota, kuten alla olevassa esimerkissä:

```
Clojure (after? (local-date 2019 03 18) (local-date 2019 03 20))
```

Tämä palauttaa `false`, koska ensimmäinen päivämäärä ei ole jälkeen toista päivämäärää. On myös mahdollista verrata päivämääriä tunnusluvuilla, kuten vuosilla, kuukausilla ja päivillä, käyttämällä `year`, `month` ja `day-of-month` funktioita. Katso lisää esimerkkejä Clojure-verkosta löytyvistä oppimisresursseista.

## Syväsyöksy

Clojurella on laaja valikoima työkaluja ja funktioita, jotka liittyvät päivämäärien käsittelyyn. Tämä sisältää myös luokat kuten `LocalDate`, `LocalDateTime` ja `Period`, jotka tarjoavat tarkempia vaihtoehtoja päivämäärien vertailuun ja käsittelyyn. Suosittelemme tutkimaan tarkemmin Clojure'n virallisen dokumentaation ja alan hyväksyttyjen käytäntöjen lähteitä.

## Katso myös

- [Clojure-päivämäärät ja aikaleimat](https://clojuredocs.org/clojure.core/LocalDate)
- [Päivämäärien vertailu ja laskenta Clojurescriptillä](https://github.com/hyPiRion/first-last-predicates)
- [Kattava opas päivämäärien käsittelyyn Clojurella](https://code.tutsplus.com/tutorials/clojure-from-the-ground-up-dates-and-times--cms-24949)