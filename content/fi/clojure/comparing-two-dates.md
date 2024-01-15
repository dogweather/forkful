---
title:                "Vertailu kahden päivämäärän välillä."
html_title:           "Clojure: Vertailu kahden päivämäärän välillä."
simple_title:         "Vertailu kahden päivämäärän välillä."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Päivämäärien vertailu voi olla tärkeää esimerkiksi tietokantojen käsittelyssä, jossa täytyy tarkastella eri päivämäärien välisiä suhteita ja järjestystä.

## Miten tehdä

Päivämäärien vertailu Clojurella on helppoa ja intuitiivista. Voit käyttää Clojuren date-luokkaa ja sen tarjoamia metodeja. Alla on esimerkkikoodi, joka vertaa kahta päivämäärää ja tulostaa sen mukaan, kumpi on aikaisempi.

```Clojure
(import java.time.LocalDate)

(def date1 (LocalDate/parse "2020-01-01"))
(def date2 (LocalDate/parse "2020-01-15"))

;; Vertaa päivämääriä ja tulosta tulos
(println (str "Päivämäärä 1: " date1))
(println (str "Päivämäärä 2: " date2))

(if (.isAfter date1 date2)
  (println "Päivämäärä 1 on myöhempi")
  (println "Päivämäärä 2 on myöhempi"))
```

Tulostus:

```
Päivämäärä 1: 2020-01-01
Päivämäärä 2: 2020-01-15
Päivämäärä 2 on myöhempi
```

## Syvällisempi sukellus

Päivämäärien vertailu Clojurella perustuu Java 8:ssa esiteltyyn java.time -pakettiin. Tässä paketissa on date-luokka, johon Clojuren date-funktio perustuu. Date-luokasta löytyy useita käteviä metodeja, kuten "isBefore", "isEqual" ja "isAfter", joilla voi tarkastella päivämäärien suhdetta toisiinsa.

On myös tärkeää huomata, että Clojuren date-funktio palauttaa päivämäärä-objektin, joka ei ole muokattavissa. Jos haluat muokata päivämäärää, voit käyttää "with" -metodia, joka palauttaa uuden päivämäärä-objektin halutulla päivämäärällä. Esimerkiksi, jos haluat lisätä yhden päivän päivämäärään, voit käyttää "withDayOfMonth" -metodia seuraavasti:

```Clojure
(def date (LocalDate/parse "2020-01-01"))
(def uusi-date (.withDayOfMonth date 2))

;; Uusi päivämäärä on nyt 2020-01-02
(println uusi-date)
```

## Katso myös

- [Java 8:n java.time -paketti](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojuren date-funktio](https://clojuredocs.org/clojure.java-time/date)