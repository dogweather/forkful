---
title:                "Clojure: Nykyisen päivämäärän hakeminen"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
On monia eri syitä, miksi haluat tietää tämänhetkisen päivämäärän. Ehkä haluat tulostaa sen johonkin sovellukseen tai käyttää sitä laskennassa. Riippumatta siitä, miksi tarvitset päivämäärän, Clojure tekee sen saamisen helpoksi.

## Miten
Voit saada tämänhetkisen päivämäärän Clojurella yksinkertaisesti käyttämällä funktiota `now`, joka löytyy `clojure.java.time` kirjastosta. Tämän funktion tulos on `java.time.LocalDateTime` objekti, joka sisältää tämänhetkisen päivämäärän ja ajan.

```Clojure
(require '[clojure.java.time :as t])

(def today (t/now))
(print today)
```

Tämän koodinpätkän tuloste olisi esimerkiksi `#object[java.time.LocalDateTime 0x4cd9992b "2021-11-24T13:37:00.540908500"]`, joka sisältää tiedon tämänhetkisestä päivämäärästä ja ajasta.

## Syvällisempi sukellus
`now` funktiolla on myös muutamia valinnaisia parametreja, joiden avulla voit määrittää tietyn aikavyöhykkeen tai kellonajan. Voit myös käyttää muita funktioita, kuten `today`, `yesterday` ja `tomorrow`, jotka ovat hyödyllisiä erilaisten päivämäärien saamiseksi.

Päivämäärä- ja aikatyypit ovat tärkeitä Clojuressa, koska niiden avulla voit tehdä tarkkoja laskutoimituksia ja muutoksia aikaan.

## Katso myös
- Clojure virallinen dokumentaatio: https://clojure.org/
- Lyhyt opas Clojuren aloittamiseen: https://clojure.org/guides/getting_started
- Elämä aikoina: Päivämäärä- ja aikatyypit Clojuressa: https://clojure.org/reference/java_time