---
title:    "Clojure: Päivämäärien vertailu"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Vertailla kahden päivämäärän välillä on tärkeä osa ohjelmointia, sillä se auttaa meitä ymmärtämään aikaa ja päivämäärien suhdetta toisiinsa. Tämä taito on erityisen hyödyllinen projektien aikataulutuksen ja tapahtumien järjestämisen yhteydessä.

## Miten

Käyttämällä Clojure-kieltä, voimme helposti vertailla kahta päivämäärää käyttäen seuraavia funktioita:

```Clojure
(require '[clj-time.core :as t])

(def date1 (t/date-time 2020 12 31))
(def date2 (t/date-time 2021 1 1))

(def compared (t/compare date1 date2))
(println "Date 1 is" compared "to Date 2.")
```

Tämä koodi palauttaa tulosteen "Date 1 is less than Date 2", mikä tarkoittaa, että ensimmäinen päivämäärä on aikaisempi kuin toinen. Voimme myös käyttää muita funktioita, kuten `between?`, joka tarkistaa ovatko kaksi päivämäärää välillä:

```Clojure
(def between (t/between? date1 date2 date3))
(println "Date 3 is between Date 1 and Date 2:" between)
```

Tämä tulostaa "Date 3 is between Date 1 and Date 2: true".

## Syvällinen sukellus

Clojure tarjoaa myös monia muita hyödyllisiä funktioita päivämäärien vertailuun, kuten `after?`, `before?` ja `duration`, jotka kaikki auttavat meitä ymmärtämään ja manipuloimaan päivämääriä ja aikoja. On myös tärkeää huomata, että Clojure käyttää Joda Time -kirjastoa päivämäärien käsittelyyn, mikä antaa meille lisävoimaa ja joustavuutta verrattuna muihin ohjelmointikieliin.

## Katso myös

- [Clojure-päivämäärien vertailun dokumentaatio](https://clojure.org/api/clj-time/4.0.0/clj-time.core#var-compare)
- [Joda Time -kirjaston dokumentaatio](https://www.joda.org/joda-time/)
- [Clojure oppiminen -opas suomeksi](https://clojure.org/guides/learn/syntax)