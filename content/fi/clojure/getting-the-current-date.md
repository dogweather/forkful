---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:13:41.303102-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Clojuressa nykyisen päivämäärän hakeminen tarkoittaa, että ohjelma noutaa reaaliajassa olevan päivämäärän tiedot. Tätä tehdään, kun tarvitaan käyttäjän toiminnan aikaleimausta tai päivämääräriippuvaista logiikkaa.

## How to:
Kuinka tehdään:

```clojure
;; Tuodaan Java-kirjaston luokat käyttöön
(import 'java.util.Date)
(import 'java.text.SimpleDateFormat)

;; Luo Date-instanssi, joka edustaa nykyhetkeä
(def now (Date.))

;; Tulosta nykyhetken päivämäärä oletusmuodossa
(println now) ; esim. output: Thu Apr 06 15:20:51 EEST 2023

;; Määritetään päivämäärän esitysmuoto
(def format (SimpleDateFormat. "dd-MM-yyyy"))

;; Muunnetaan ja tulostetaan päivämäärä halutussa muodossa
(println (.format format now)) ; output: 06-04-2023
```
Koodiesimerkki luo nykyisen päivämäärän ja tulostaa sen sekä oletusmuodossa että määritellyssä muodossa.

## Deep Dive
Sukellus syvemmälle:

Clojure on Lisp-perheen kieli, joka toimii JVM:ssä (Java Virtual Machine). Sen vuoksi päivämäärät käsitellään usein hyödyntäen Javan valmiita luokkia kuten `java.util.Date`. Historiallisesti Lisp-kielet ovat olleet vahvoilla symbolisen laskennan ja metaprogrammoinnin alueilla, ja Clojure jatkaa tätä perinnettä funktionaalisen ohjelmoinnin maailmassa.

Vaihtoehtoisina keinoja daten käsittelyyn Clojuressa olisi käyttää Joda-Time-kirjastoa (ennen Java 8:aa) tai `java.time` pakkauksen luokkia (Java 8 ja uudemmat). Esimerkiksi:

```clojure
;; Java 8:n java.time kirjastoa käyttäen
(import 'java.time.LocalDate)
(import 'java.time.format.DateTimeFormatter)

;; Nykyinen paikallinen päivämäärä
(def today (LocalDate/now))

;; Tulostetaan nykyinen päivämäärä
(println today) ; output: 2023-04-06

;; Määritellään formatteri
(def formatter (DateTimeFormatter/ofPattern "dd-MM-yyyy"))

;; Tulostetaan muotoiltu päivämäärä
(println (.format formatter today)) ; output: 06-04-2023
```

Clojuressa päivämäärän käsittelyyn on tarjolla lukuisia kirjastoja, jotka mahdollistavat monipuoliset manipulaatiot ja lähestymistavat ajanhallintaan.

## See Also
Katso myös:

- Clojure-dokumentaatio: [https://clojure.org/](https://clojure.org/)
- Java 8 Date and Time API -esimerkkejä: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- Joda-Time -projektin kotisivu: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
- Clojure for the Brave and True, luku päivämääristä ja ajasta: [https://www.braveclojure.com/do-things/](https://www.braveclojure.com/do-things/)
