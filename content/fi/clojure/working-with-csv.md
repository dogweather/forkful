---
title:                "CSV-tiedostojen käsittely"
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV eli Comma-Separated Values on tiedostomuoto, jossa dataa tallennetaan pilkulla erotettuina arvoina. Ohjelmoijat käsittelevät CSV-tiedostoja, koska ne ovat yksinkertaisia, ihmisen luettavia ja helppoja jakaa eri järjestelmien välillä.

## How to:
Clojuven avulla CSV-tiedoston lukeminen ja kirjoittaminen on suoraviivaista. Katsotaanpa esimerkkejä.

### CSV:n lukeminen
```Clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "data.csv")]
  (doall (csv/read-csv reader)))
```

### CSV:n kirjoittaminen
```Clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["otsake1", "otsake2"] ["data1", "data2"]]))
```

## Deep Dive
CSV-formaatti on vanha; se on käytetty ensimmäisen kerran jo 1970-luvulla. Vaihtoehtoisia tiedostomuotoja ovat esimerkiksi JSON ja XML, jotka ovat rakenteellisesti monimutkaisempia. Clojuressa CSV-tiedostojen käsittely nojaa kirjastoon `clojure.data.csv`, joka hoitaa rivien jäsentämisen ja niiden käsittelemisen kokoelmina.

## See Also
- Clojure.data.csv GitHub: https://github.com/clojure/data.csv
- Clojuressa CSV-tietojen käsittelyn dokumentaatio: https://clojuredocs.org/clojure.data.csv
- CSV-määritelmä RFC 4180: https://tools.ietf.org/html/rfc4180