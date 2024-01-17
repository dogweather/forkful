---
title:                "Päivämäärän hankkiminen"
html_title:           "Clojure: Päivämäärän hankkiminen"
simple_title:         "Päivämäärän hankkiminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Saada nykyinen päivämäärä tarkoittaa, että ohjelma hakee ja näyttää tietokoneen tai käyttäjän aikavyöhykkeen mukaisen päivän ja kellonajan. Tämä on tärkeää, sillä päivämäärän käyttö mahdollistaa aikapohjaisen laskennan, kuten maksujen suorittamisen tai kalenteritoimintojen toimimisen. Ohjelmoijat tarvitsevat tätä toimintoa erilaisissa sovelluksissa ja ohjelmoinnissa.

## How to:
```Clojure
(import java.time.LocalDate)

; hae nykyinen päivämäärä ja tulosta se
(let [nykyinen-päivä (LocalDate/now)]
  (println nykyinen-päivä))

; lisää päiviä nykyiseen päivään
(let [tuleva-päivä (LocalDate/now)]
  (println (.plusDays tuleva-päivä 10)))

; muokkaa nykyistä päivämäärää
(let [päivä (LocalDate/now)]
  (println (.withDayOfMonth päivä 15)))
```

Esimerkkitulos:

2021-05-05
2021-05-15
2021-04-15

## Deep Dive:
Saadaksemme nykyisen päivämäärän Clojurella, voimme käyttää Java.time-pakettia, jota Clojure tukee. Tämä paketti sisältää monia hyödyllisiä luokkia ja metodeja, kuten LocalDate-luokan, joka mahdollistaa päivämäärän käsittelyn. Myös Joda-time-kirjasto on usein käytetty vaihtoehto, joka tarjoaa samanlaisia toimintoja kuin Java.time.

Saadaksemme tarkempia tietoja nykyisen päivämäärän hankinnasta, voi olla hyödyllistä tutustua Java.time-luokkaan ja sen saatavilla oleviin metodeihin. On myös tärkeää huomata, että päivämäärän hankintaan liittyy myös aikavyöhykkeiden käsittelemistä, mikä vaikuttaa tulokseen eri käyttöympäristöissä.

## See Also:
- [Java.time-paketti Clojure-dokumentaatiossa](https://clojure.org/reference/java_interop#_date_and_time)
- [Joda-time-kirjasto](https://www.joda.org/joda-time/)