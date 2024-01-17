---
title:                "Kahden päivämäärän vertailu"
html_title:           "Clojure: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Päivämäärien vertailu tarkoittaa kahden päivämäärän välisen eron tai suhteen selvittämistä. Ohjelmoijat tekevät tätä esimerkiksi tarkastaessaan kahden tapahtuman välistä aikaa tai laskiessaan eron kahden henkilön syntymäajan välillä.

## Miten:
```Clojure
(require '[clj-time.core :as time])
;; Käytetään clj-time -kirjastoa päivämäärien käsittelyyn

(def today (time/today))
(def tomorrow (time/plus-days today 1))

(time/days-diff today tomorrow) ;; Output: -1
;; Palauttaa päivien määrän, joka on erotus kahden päivämäärän välillä

(time/after? today tomorrow) ;; Output: false
;; Tarkistaa, onko päivämäärä today jälkeen tomorrow

```

## Syvällinen sukellus:
Päivämäärien vertailu on ollut tärkeä osa ohjelmointia jo pitkään. Aiemmin manuaalinen laskeminen oli ainoa tapa tehdä se, mutta nykyään on olemassa useita kirjastoja, kuten clj-time, jotka tekevät prosessin helpoksi ja tarkaksi. Joissakin ohjelmointikielissä, kuten Java, päivämääräluokat ovat osa ydintoimintoja, kun taas toisissa se voi olla lisäominaisuus. Tällöin tietojen välittäminen muista kielistä, kuten penkojen databasesista, voi olla vaikeaa ja saada aikaan virheitä päivämäärien muotoilussa.

## Katso myös:
[clj-time-kirjasto](https://github.com/clj-time/clj-time)