---
title:                "Päivämäärän erottelu merkkijonosta"
html_title:           "Clojure: Päivämäärän erottelu merkkijonosta"
simple_title:         "Päivämäärän erottelu merkkijonosta"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Päivämäärän parsiminen merkkijonosta tarkoittaa sen muuntamista tekstimuodosta päivämääräobjektiksi, jota tietokone voi käsitellä. Tämä on tärkeää, koska ohjelmissa käytetään usein päivämäärätietoa, joka tulee ulkoisesta tietolähteestä merkkijonona.

## Ohjeet:
```Clojure
(require '[clojure.string :as string])
(require '[clojure.java-time :as t])

;; Parsi päivämäärä päiväysmuodosta.
(t/parse str "2018-12-06")  ;; => #object[java.time.LocalDate 0x75a6ee41 "2018-12-06"]

;; Parsi aika aikaleimanmuodosta.
(t/parse-time str "13:45:30")  ;; => #object[java.time.LocalDateTime 0x57396a42 "2019-02-19T13:45:30"]
```

## Syvempi sukellus:
Päivämäärän parsiminen merkkijonosta on ollut haastava tehtävä ohjelmoinnissa jo pitkään. Aiemmin tälle tarpeelle kehitettiin monimutkaisia algoritmeja, mutta nykyään onneksi löytyy helppokäyttöisiä kirjastoja kuten Java-Time Clojure-kirjasto.

Jos haluat lukea lisää päivämäärän parsimisesta, suosittelemme tutustumaan LocalDateTimeAPI:n dokumentaatioon sekä Java-Time-tutoriaaleihin.

## Katso myös:
- [Java-Time Clojure-kirjasto](https://github.com/java-time/java-time)
- [LocalDateTimeAPI:n dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java-Time -tutoriaalit](https://www.baeldung.com/java-8-date-time-intro)