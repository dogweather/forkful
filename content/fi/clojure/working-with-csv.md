---
title:                "Töitä CSV:n kanssa"
html_title:           "Clojure: Töitä CSV:n kanssa"
simple_title:         "Töitä CSV:n kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot ovat yleinen tapa tallentaa ja jakaa taulukkotietoja, kuten tietokantoja ja Excel-laskentataulukoita. Clojuren avulla voit helposti lukea ja muokata näitä tiedostoja, mikä tekee siitä hyödyllisen ohjelmointikielen ymmärtää.

## Miten tehdä se

CSV-tiedostot voidaan lukea ja muokata käyttämällä `clojure.data.csv` -kirjastoa. Ensimmäiseksi sinun on tuotava tämä kirjasto käyttämällä `require` -komentoa:

```Clojure
(require '[clojure.data.csv :as csv])
```

Seuraavaksi lue CSV-tiedosto käyttämällä `with-open` -makroa ja `csv/reader` -funktiota:

```Clojure
(with-open [reader (csv/reader "tiedostonimi.csv")]
  (doseq [row reader]
    (println row)))
```

Tämä tulostaa jokaisen rivin tiedostosta konsoliin. Voit myös tallentaa rivit vektoriin ja käsitellä niitä myöhemmin:

```Clojure
(with-open [reader (csv/reader "tiedostonimi.csv")]
  (let [rows (doall reader)])
    ; tehdä jotain vektoreille täällä
  ))
```

Voit myös muokata CSV-tiedostoa ja tallentaa uuden version käyttämällä `csv/writer` -funktiota:

```Clojure
(with-open [reader (csv/reader "tiedostonimi.csv")]
  (with-open [writer (csv/writer "uusitiedostonimi.csv")]
    (doseq [row reader]
      (csv/write-row writer (map #(str (inc %)) row)))))
```

Tässä esimerkissä kutsumme `inc` -funktiota jokaiselle riville ja tallennamme tulokset uuteen CSV-tiedostoon.

## Syvä sukellus

CSV tiedostot koostuvat yleensä otsikkorivista ja sen jälkeen datariveistä. Voit ottaa huomioon otsikon käyttämällä `csv/with-header` -funktiota:

```Clojure
(with-open [reader (csv/with-header "tiedostonimi.csv" :header true)]
  (doseq [row reader]
    (println (:sarakkeenimi row))))
```

Tämä antaa sinulle mahdollisuuden käsitellä tiedoston sarakkeita nimillä sen sijaan, että viitataan sarakkeiden indekseihin.

Voit myös lukea CSV-tiedoston ilman otsikkoa ja määrittää sarakkeiden nimet myöhemmin käyttämällä `csv/named-readers` -funktiota:

```Clojure
(with-open [reader (csv/named-readers "tiedostonimi.csv"
                                      ["sarakkeenimi1", "sarakkeenimi2"])]
  (doseq [row reader]
    (println (:sarakkeenimi1 row))))
```

Tämä lähestymistapa antaa sinulle suuremman hallinnan tiedoston sisältöön.

Lisätietoja CSV-tiedostojen muotoilusta ja käyttäytymisestä löytyy [Clojuren virallisesta dokumentaatiosta](https://clojure.github.io/data.csv/).

## Katso myös

- [Clojuren virallinen dokumentaatio](https://clojure.org/)
- [Clojure-toolkitti CSV-tiedostojen käsittelyyn](https://github.com/clojure-toolkit/csv)