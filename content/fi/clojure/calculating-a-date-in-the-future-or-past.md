---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "Clojure: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Laskeminen pvm tulevaisuudessa tai menneisyydessä tarkoittaa päivien, viikkojen tai vuosien lisäämistä tai poistamista nykyisestä päivämäärästä. Ohjelmoijat tekevät tätä esimerkiksi luodakseen toistorakenteita, tiedonkeruusovelluksia tai aikatauluttamisjärjestelmiä.

## Kuinka:
```
Clojure (-> (java.util.Date.) (.toString))
=> "Mon Jun 29 16:15:09 EDT 2020" 

;; Lisätään yksi päivä
Clojure (-> (java.util.Date.) (.getTime) (+ (* 86400000 1)) (java.util.Date.))
=> "Tue Jun 30 16:15:09 EDT 2020" 

;; Vähennetään 3 päivää
Clojure (-> (java.util.Date.) (.getTime) (- (* 86400000 3)) (java.util.Date.))
=> "Sat Jun 27 16:15:09 EDT 2020" 
```

## Syventymiskurssi:
Ohjelmistoteollisuudessa pvm laskeminen on tärkeä osa erilaisten sovellusten, kuten aikataulutusjärjestelmien, toteuttamista. Clojurella pvm laskenta on helppoa, sillä kieli tarjoaa valmiita funktioita pvm tietojen manipulointiin.
Vaihtoehtoisia tapoja suorittaa pvm laskentaa ovat esimerkiksi Java-kirjaston käyttäminen tai oma funktiojen kirjoittaminen Clojurella.
Tarkempi implementaation kuvaus löytyy Clojure-kirjastojen dokumentaatiosta.

## Katso myös:
- [Clojure Date And Time Tutorial] (https://clojure.org/guides/core_time)
- [Java Calendar API] (https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Clojure Date Implementation Details] (https://clojuredocs.org/clojure.core/date)