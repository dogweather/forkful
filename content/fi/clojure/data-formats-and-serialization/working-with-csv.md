---
title:                "Työskentely CSV:n kanssa"
aliases:
- fi/clojure/working-with-csv.md
date:                  2024-02-03T19:19:49.209774-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

CSV-tiedostojen (pilkuilla erotetut arvot) käsittely sisältää tekstidatan jäsentämisen ja generoinnin riveinä ja sarakkeina, samankaltaisesti kuin taulukkolaskentadatassa. Tämä prosessi on olennainen tiedonvaihdossa sovellusten, tietokantojen välillä ja datan muuntamistehtävissä, CSV:n laajan käytön vuoksi kevyenä, yhteensopivana muotona.

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
