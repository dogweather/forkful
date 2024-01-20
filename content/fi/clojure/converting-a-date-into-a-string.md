---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Muunna päivämäärä merkkijonoksi tarkoittaa päivämäärän esittämistä tekstimuodossa. Ohjelmoijat tekevät tämän helpottaakseen päivämäärien käsittelyä ja esittämistä loppukäyttäjille.

## Kuinka:

Clojure käyttää java.time.bibliotekan funktioita päivämäärien muuntamiseen. Seuraava esimerkki näyttää, kuinka muunnat `LocalDate`-objektin merkkijonoksi.

```Clojure 
(require '[java.time :as jt])

(defn format-date
  [date]
  (.format date (jt/DateTimeFormatter/ofPattern "dd.MM.yyyy")))

(format-date (jt/LocalDate/of 2021 12 1))
```

Koodin suorittaminen tuottaa seuraavan tulosteen:

```Clojure
"01.12.2021"
```

## Sukellus syvemmälle:

Historiallisessa yhteydessä päivämäärät ovat aina olleet vaikeasti käsiteltäviä ohjelmoinnissa. Java lanseerasi java.time-bibliotekan JDK 8:ssa vanhempien, monimutkaisten päivämääräkirjastojen korvaamiseksi.

Alternatiivisesti, voit käyttää `format`-funktiota clojure.contrib.str-bibliotekasta seuraavalla tavalla:

```Clojure 
(require '[clojure.contrib.str :as str])

(defn format-date-alt
  [date]
  (str/format "%1$td.%1$tm.%1$tY" date))

(format-date-alt (jt/LocalDate/of 2021 12 1))
```

Joka tuottaa saman tulosteen:

```Clojure
"01.12.2021"
```

Ja suorituskykyä ajatellen, java.time.bibliotekan funktioita suositellaan niiden erinomaisen suorituskyvyllisyyden takia.

## Katso myös:

Vieraile seuraavissa lähteissä saadaksesi lisää apua ja tietoa:

- [Clojure for the Brave and True: Dates and Time](https://www.braveclojure.com/core-functions-in-depth/#Dates_and_Times)
- [Java SE 8 Date and Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure.contrib.str documentation](https://clojuredocs.org/clojure.contrib.str/format)