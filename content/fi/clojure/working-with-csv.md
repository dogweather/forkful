---
title:                "Clojure: Csv:n kanssa työskentely"
simple_title:         "Csv:n kanssa työskentely"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi


Miksi joku haluaisi käyttää CSV-tiedostoja Clojure-ohjelmoinnissa? CSV-tiedostot ovat yleinen tapa tallentaa dataa taulukkomuodossa, ja Clojure tarjoaa joukon käteviä työkaluja tällaisten tiedostojen käsittelyyn.

## Kuinka käsitellä CSV-tiedostoja Clojurella

Kun haluat käsitellä CSV-tiedostoja Clojurella, sinun täytyy ensin tuoda `clojure.data.csv` kirjasto projektisi riippuvuuksiin. Sitten voit lukea CSV-tiedoston käyttämällä `clojure.data.csv` kirjaston `read-csv` funktiota. Esimerkiksi, jos haluat lukea tiedoston nimeltä "data.csv", voit tehdä seuraavaa:

```Clojure
(ns tiedostonimi
  (:require [clojure.data.csv :as csv]))

(def data (csv/read-csv "data.csv"))
```

Tämä luo data-rakenteen, joka sisältää kaiken tiedoston sisällön. Tässä vaiheessa voit käsitellä dataa haluamallasi tavalla, esimerkiksi tulostamalla sen komentoriville:

```Clojure
(doseq [rivi data]
  (println rivi))
```

#### Taulukkomuotoinen tieto

Kun luet CSV-tiedostoa Clojurella, data-rakenne sisältää jokaisen rivin omana listanaan. Tämä tarkoittaa sitä, että jokainen sarakkeen arvo on yksittäisenä alkiona listassa. Esimerkiksi, jos tiedostosi sisältää seuraavat rivit:

```csv
nimi, ikä, sukupuoli
Matti, 34, mies
Sari, 28, nainen
```

data-rakenteesi näyttää tältä:

```Clojure
(["nimi" "ikä" "sukupuoli"]
 ["Matti" "34" "mies"]
 ["Sari" "28" "nainen"])
```

#### CSV-tiedoston kirjoittaminen

Voit myös käyttää `clojure.data.csv` kirjastoa CSV-tiedoston kirjoittamiseen. Tässä esimerkki, jossa luodaan uusi CSV-tiedosto ja kirjoitetaan siihen dataa:

```Clojure
(ns tiedostonimi
  (:require [clojure.data.csv :as csv]))

(def data [["nimi" "ikä" "sukupuoli"]
           ["Matias" "31" "mies"]
           ["Anni" "26" "nainen"]])

(csv/write-csv "uusi_tiedosto.csv" data)
```

Tämä luo uuden tiedoston nimeltä "uusi_tiedosto.csv" samassa kansiossa, ja sen sisältö on seuraava:

```csv
nimi,ikä,sukupuoli
Matias,31,mies
Anni,26,nainen
```

## Syväsukellus

Clojure tarjoaa monia muita hyödyllisiä toimintoja CSV-tiedostojen käsittelyyn, kuten `parse-csv`, `print-csv`ja `write-csv`. Voit tutustua näihin toimintoihin Clojuren virallisen dokumentaation avulla.

## Katso myös

- [Clojuren virallinen dokumentaatio](https://clojure.org/reference/laziness)
- [clojure.data.csv -dokumentaatio](https://clojure.github.io/data.csv/)