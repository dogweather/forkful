---
title:    "Clojure: Nykyisen päivämäärän hakeminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku haluaisi saada nykyisen päivämäärän Clojure-ohjelmassa. Ehkä päivämäärää tarvitaan laskelmien tekemiseen tai tietojen tallentamiseen tietokantaan.

## Miten

On olemassa useita tapoja saada nykyinen päivämäärä Clojure-ohjelmassa. Yksi tapa on käyttää Clojuren `java.time` -kirjastoa. Voit käyttää `LocalDate` -luokkaa saadaksesi nykyisen päivämäärän, ja `LocalDateTime` -luokkaa saadaksesi myös ajan. Voit kutsua näitä luokkia seuraavalla tavalla:

```Clojure
(require '[java.time :as time])

(time/LocalDate/now)          ; Palauttaa nykyisen päivämäärän
(time/LocalDateTime/now)      ; Palauttaa nykyisen päivämäärän ja ajan
```

Tämä palauttaa `LocalDate` - tai `LocalDateTime` -tyyppiset oliot, jotka voit tallentaa muuttujaan ja käyttää tarvittaessa.

Voit myös käyttää Clojuren `clj-time` -kirjastoa, joka tarjoaa käteviä funktioita päivämäärän muotoiluun. Voit asentaa kirjaston Leiningenillä seuraavalla komennolla:

```
lein deps :tree
```

Sitten voit käyttää `clj-time.core` -kirjastoa seuraavalla tavalla:

```Clojure
(require '[clj-time.core :as time])

(time/today)           ; Palauttaa nykyisen päivämäärän muodossa #inst "yyy-mm-ddT00:00:00.000-00:00"
(time/now)             ; Palauttaa nykyisen päivämäärän ja ajan muodossa #inst "yyy-mm-ddThh:mm:ss.000-00:00"
```

## Syvällinen sukellus

Tarkastelemme nyt tarkemmin, mitä todellisuudessa tapahtuu, kun kutsut `java.time` -kirjaston funktioita. Clojure-ohjelmassa voit hyödyntää Javan omia luokkia ja metodeja nimiavaruuden `java.*` kautta. `java.time` -kirjaston toiminnot palauttavat Javan `LocalDate` ja `LocalDateTime` -luokkien ilmentymiä. Näiden luokkien avulla voit manipuloida päivämääriä ja aikoja, esimerkiksi lisäämällä päiviä tai muuttamalla aikavyöhykettä.

Jos haluat muotoilla päivämäärän tai ajan tietyn tyylisenä, voit käyttää `java.time.format` -nimiavaruutta. Esimerkiksi jos haluat muotoilla nykyisen päivämäärän muotoon "d.M.yyyy", voit tehdä sen seuraavasti:

```Clojure
(require '[java.time :as time])
(require '[java.time.format :as format])

(def now (time/LocalDate/now))

(time/format now "d.M.yyyy")    ; Palauttaa nykyisen päivämäärän muodossa "päivä.kuukausi.vuosi"
```

## Katso myös

- [Java.time Javadoc](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Clojure.java-time dokumentaatio](https://clojure.github.io/java-time/)
- [Clj-time dokumentaatio](https://github.com/clj-time/clj-time)