---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:49.209774-07:00
description: 'Kuinka: #.'
lastmod: '2024-03-13T22:44:56.206630-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

## Kuinka:


### CSV-tiedoston lukeminen
Clojurella ei ole sisäänrakennettua CSV:n jäsentämistä sen vakio-kirjastossa, mutta voit käyttää `clojure.data.csv` -kirjastoa tähän tarkoitukseen. Lisää ensin kirjasto projektisi riippuvuuksiin.

`project.clj`-tiedostoosi, lisää seuraava riippuvuus:
```clojure
[clojure.data.csv "1.0.0"]
```
Lue CSV-tiedosto ja tulosta jokainen rivi:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "polku/tiedostoosi.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Tämä tulostaa jokaisen CSV:n rivin Clojure-vektorina.

### Kirjoittaminen CSV-tiedostoon
Voit kirjoittaa dataa CSV-tiedostoon käyttämällä samaa `clojure.data.csv` -kirjastoa:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "nimi" "ikä"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "polku/tulostetiedosto.csv")]
    (csv/write-csv writer data)))
```
Tämä luo tai ylikirjoittaa `tulostetiedosto.csv`, täyttäen sen määritellyllä datalla.

### Kolmannen osapuolen kirjaston käyttäminen: `clojure.data.csv`
Vaikka `clojure.data.csv` onkin ehkä suoraviivaisin kirjasto CSV-käsittelyyn Clojuressa, monimutkaisempiin tehtäviin, kuten erikoismerkkejä sisältävien CSV:iden tai epätavallisten erotinmerkkien käsittelyyn, saattaisit tutkia lisävaihtoehtoja ekosysteemissä tai jopa harkita Java-interoppia kirjastojen kuten Apache Commons CSV kanssa. Kuitenkin useimmille standardeille CSV:n käsittelytehtäville Clojuressa, `clojure.data.csv` tarjoaa yksinkertaisen ja tehokkaan työkalupakin.
