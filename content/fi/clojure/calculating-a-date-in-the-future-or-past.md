---
title:    "Clojure: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi
Monilla ohjelmoijilla saattaa olla tarve laskea päiviä menneisyydessä tai tulevaisuudessa, esimerkiksi kun luodaan sovelluksia tapahtumakalentereihin tai varauksien hallintaan.

## Miten
```Clojure
(require '[clj-time.core :as time])

; Laskee päivän perusteella tietyn määrän päiviä eteenpäin tai taaksepäin
(time/plus (time/today) {:days 7}) ; 7 päivää tulevaisuudessa
(time/minus (time/today) {:weeks 2}) ; 2 viikkoa menneisyydessä

; Laskee tietyn päivämäärän perusteella tietyn määrän päiviä eteenpäin tai taaksepäin
(time/plus (time/date 2020 1 1) {:months 2}) ; 1. maaliskuuta 2020
(time/minus (time/date 2020 3 1) {:years 5}) ; 1. maaliskuuta 2015

; Laskee nykyisen päivän ja halutun päivämäärän välisen päivien määrän
(def today (time/today)) ; tänään
(def date (time/date 2020 7 1)) ; tuleva päivämäärä
(time/in-days today date) ; tulostaa 91, eli 91 päivää tulevaisuudessa

; Huomioi myös vuoden vaihdos
(def today (time/today))
(time/in-days today (time/date 2021 1 1)) ; tulostaa 366, koska vuoden vaihduttua on yksi päivä lisää

```

## Syvempi sukellus
Clojuren clj-time kirjasto tarjoaa erinomaiset työkalut päivämäärien laskemiseen tulevaisuudessa tai menneisyydessä. Funktioita voidaan käyttää yksittäisten päivämäärien lisäksi myös ajankohtien ja aikavälien laskemiseen.

## Katso myös
- [clj-time dokumentaatio](https://github.com/clj-time/clj-time)
- [Stack Overflow - Calculating future date in Clojure with clj-time](https://stackoverflow.com/questions/38118880/calculating-future-date-in-clojure-with-clj-time)
- [Clojure Cheatsheet - Dates and Times](https://clojure.org/api/cheatsheet/#date_and_time_manipulation)