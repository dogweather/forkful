---
title:                "Tulevaisuuteen tai menneisyyteen päivämäärän laskeminen"
html_title:           "Clojure: Tulevaisuuteen tai menneisyyteen päivämäärän laskeminen"
simple_title:         "Tulevaisuuteen tai menneisyyteen päivämäärän laskeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit laskea päivämäärän tulevaisuudessa tai menneisyydessä? Yksi syy voisi olla esimerkiksi tarve laskea tulevan laskun eräpäivää tai tarkistaa, milloin tietty tapahtuma on tapahtunut aiemmin.

## Miten tehdä

Laskeminen tulevaisuuden tai menneisyyden päivämääriä Clojurella on helppoa käyttämällä `calendar/date-time`-kirjastoa. Voit käyttää `plus`- ja `minus`-funktioita lisäämään tai vähentämään päiviä halutusta päivämäärästä.

```Clojure
(require '[clojure.java-time :as t])
(require '[java-time.calendar :as calendar])

(def today (t/now))

(calendar/plus today (t/days 30))
;; Tulostaa päivämäärän 30 päivää tulevaisuudessa

(calendar/minus today (t/months 6))
;; Tulostaa päivämäärän 6 kuukautta taaksepäin
```

Voit myös käyttää `plus`- ja `minus`-funktioita yhdessä muiden java-time-kirjaston funktioiden kanssa, kuten `with-zone`. Tällä voit määrittää aikavyöhykkeen, jossa päivämäärä lasketaan.

```Clojure
(require '[java-time.zone :as zone])

(calendar/minus today (t/days 7) (zone/utc))
;; Laskee päivämäärän 7 päivää taaksepäin, mutta käyttää UTC-aikavyöhykettä
```

## Syvempi sukellus

`calendar/date-time`-kirjaston funktioilla laskettavat päivämäärät ovat immutaabeleja, mikä tarkoittaa, että ne eivät muutu vaan uusi päivämäärä palautetaan. Tämä tekee laskemisesta turvallisempaa, sillä et vahingossa muuta alkuperäistä päivämäärää.

Voit myös käyttää muita kirjastojen funktioita, kuten `format`, jotta saat tulostettua päivämäärän halutussa muodossa.

```Clojure
(require '[java-time.format :as format])

(def future-date (calendar/plus today (t/days 10)))
(format/formatted "dd.MM.yyyy" future-date)
;; Tulostaa päivämäärän muodossa 10.01.2022
```

## Katso myös

- [Clojure java-time -kirjasto](https://github.com/dm3/clojure.java-time)
- [Java 8 java.time API -opas](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)