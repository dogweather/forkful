---
title:                "Työskentely csv:n kanssa"
html_title:           "Haskell: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi työskennellä CSV-tiedostojen parissa? Yksinkertaisesti sanottuna, CSV-tiedostot ovat erittäin yleisessä käytössä, ja niiden käsittely on välttämätöntä monille ohjelmoijille, varsinkin jos he käsittelevät dataa.

## Miten

Haskellin avulla CSV-tiedostojen käsittely on suhteellisen helppoa ja suoraviivaista. Ensimmäiseksi sinun täytyy importata **csv** -kirjasto:

```Haskell
import Text.CSV
```

Seuraavaksi voit avata CSV-tiedoston ja tallentaa sen listaan käyttämällä **parseCSVFromFile** -funktiota:

```Haskell
records <- parseCSVFromFile "tiedosto.csv"
```

Saat nyt listan listoista, jotka sisältävät CSV-tiedoston rivit. Voit esimerkiksi tulostaa kaikki rivit käyttämällä **mapM_** -funktiota:

```Haskell
mapM_ putStrLn (map show records)
```

Jos haluat käsitellä tiettyjä arvoja tiedostosta, voit käyttää **index** -funktiota. Jos esimerkiksi haluat tulostaa ensimmäisen rivin kolmannen sarakkeen arvon, voit käyttää seuraavaa koodia:

```Haskell
let value = records !! 0 !! 2
print value 
-- Tulostaa ensimmäisen rivin kolmannen sarakkeen arvon
```

## Syventävä sukellus

CSV-tiedostot sisältävät usein tekstiä ja eri datatyyppejä. Haskellin avulla voit käsitellä näitä erilaisia datatyyppejä helposti. Esimerkiksi jos haluat muuntaa sarakkeen arvon merkkijonosta numeroarvoksi, voit käyttää **read** -funktiota:

```Haskell
let value = read (records !! 0 !! 2) :: Double
print value
-- Tulostaa ensimmäisen rivin kolmannen sarakkeen arvon muunnettuna Double-tyypiksi
```

Huomaa myös, että CSV-tiedoston alkuun voidaan lisätä ns. otsikkorivi, joka sisältää sarakkeiden nimet. Tämän avulla voit käsitellä tiedostoa helpommin, kun tiedät mitä arvoja kukin sarake sisältää.

## Katso myös

- [Haskellin dokumentaatio CSV-kirjastolle](https://hackage.haskell.org/package/csv)
- [Ohjelmointiopas CSV-tiedostojen käsittelyyn](https://www.haskell.org/haskellwiki/How_to_work_on_CSV_data)