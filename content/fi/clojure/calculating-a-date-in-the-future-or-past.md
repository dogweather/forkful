---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Clojure: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Clojure ja päivämäärän laskeminen tulevaisuudessa tai menneisyydessä

## Mitä & Miksi?
Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä tarkoittaa tietyn ajanjakson laskemista nykyisestä hetkestä eteenpäin tai taaksepäin. Ohjelmoijat tekevät tämmöistä usein esimerkiksi aikarajojen, eräpäivien ja vanhenemisajankohtien laskemiseksi.

## Miten se tehdään:
```Clojure
;; Tuodaan java.util.Date ja java.util.concurrent.TimeUnit kirjastot
(import [java.util Date]
        [java.util.concurrent TimeUnit])

(defn muunna-paivaiksi [paivat]
  ;; Muunnetaan päivät millisekunneiksi
  (TimeUnit/DAYS.toMillis paivat))

(defn laske-paivamaara [suunta]
  ;; Luo uusi Date olio nykyisestä ajasta
  (let [nykyhetki (Date.)
        aikavali (muunna-paivaiksi suunta)]
    (Date. (+ (.getTime nykyhetki) aikavali))))
```
Esimerkki output:
```Clojure
user> (laske-paivamaara 7)
#inst "2023-04-29T16:06:38.334-00:00"
```
Tämä Clojure funktio luo uuden päivämäärän, joka tulee olemaan 7 päivää nykyhetken jälkeen.

## Syvennys
Historiallisesti päivämäärän laskeminen tulevaisuudessa tai menneisyydessä voi olla monimutkaista, koska eri kalenterijärjestelmät määrittävät "päivän" eri tavoin. Clojure, kuten useimmat modernit ohjelmointikielet, hyödyntää Java's Date and Time API:ta, joka antaa joustavuuden ja tarkan kontrollin ajan laskemiseen.

Clojurella on myös erinomaisia kirjastoja, kuten clj-time, jotka tarjoavat hyödyllisiä abstrakteja ajan ja päivämäärän käsittelyyn.

Parasta Clojuressa on sen joustavuus ja yksinkertaisuus. Voit luoda oman funktion päivämäärän laskemiseen tai käyttää valmiita kirjastoja.

## Katso myös
1. Clojure for the Brave and True: [Ajan ja päivämäärän käsittely](https://www.braveclojure.com/core-functions-in-depth/)
3. Clj-time GitHub: [clj-time:n dokumentaatio](https://github.com/clj-time/clj-time)