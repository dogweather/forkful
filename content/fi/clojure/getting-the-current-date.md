---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Nykyinen päivämäärä tarkoittaa sitä, että saadaan selville tämänhetkinen päivä seuraavista tiedoista: vuosi, kuukausi ja päivä. Tämä on hyödyllistä monissa ohjelmointitilanteissa, esimerkiksi ajan seurannassa ja lokitiedoissa.

# Kuinka:

Näillä muutamilla Clojure-rivikoodeilla saadaan nykyinen päivämäärä:

```Clojure
(ns clojure.examples
  (:require [clj-time.core :as t]))

(defn now []
  (t/now))
```
Kuten näette, `now`-funktio palauttaa nykyisen päivämäärän ja ajan.

Kun suoritat tämän koodin, saat tulokseksi esimerkiksi:

```Clojure
#object[org.joda.time.DateTime 2022-03-15T12:02:18.012Z]
```

# Syvällisemmin:

Historiallisesti, päivämäärän ja ajan hankkiminen on yleensä ollut yksinkertainen toiminto suurimmissa ohjelmointikielissä.

Kuitenkin, kuten monissa muissakin ohjelmointikysymyksissä, on olemassa useita vaihtoehtoja. Voit käyttää Java's Date- ja Calendar-luokkia tai Joda-Time-kirjastoa (kuten esimerkissä), josta on tullut standardi Clojuren käyttäjille.

Implementation details tarkoittaa siis valintaasi kirjaston ja funktion välillä. Joka tapauksessa, sovelluksesi tarpeet määrittelevät sen.

# Katso myös:

Clojuren dokumentaatio päivämäärästä ja ajasta: [Linkki](https://clojuredocs.org/clj-time.core/now)
Java Date ja Calendar luokkien dokumentaatio: [Linkki](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html), [Linkki](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
Joda-Time kirjaston dokumentaatio: [Linkki](https://www.joda.org/joda-time/)