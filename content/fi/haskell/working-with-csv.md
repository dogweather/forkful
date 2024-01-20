---
title:                "Työskentely .csv:n kanssa"
html_title:           "Haskell: Työskentely .csv:n kanssa"
simple_title:         "Työskentely .csv:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
CSV-tiedostot (Comma-Separated Values) ovat yleisesti käytettyjä tietomuotoja, jotka mahdollistavat dataa sisältävän taulukon tallentamisen yksinkertaisessa tekstimuodossa. Tätä tietomuotoa käytetään usein, koska se on helppo lukea ja ymmärtää sekä soveltuu hyvin tietojen jakamiseen eri ohjelmistojen ja järjestelmien välillä.

## Kuinka?
Käyttäen Haskellia voimme helposti työskennellä CSV-tiedostojen kanssa. Alla on esimerkki, joka lukee ja tulostaa CSV-tiedoston sisällön.

```Haskell
import Text.CSV

main = do
  let fileName = "testi.csv"
  csvData <- parseCSVFromFile fileName
  case csvData of
    Left err -> putStrLn "Tiedoston lukeminen epäonnistui"
    Right rows -> mapM_ (putStrLn . show) rows
  ```
  Tuloste:

  ```
  ["Tieto1", "Tieto2", "Tieto3"]
  ["1", "2", "3"]
  ["4", "5", "6"]
  ```
  Koodin ensimmäinen rivi tuo Text.CSV-moduulin, joka sisältää työkalut CSV-tiedoston käsittelyyn. Seuraavaksi määritellään pääfunktio `main`, joka lukee tiedoston ja tulostaa sen sisällön. Tulosteen muotoilun hoitaa `mapM_`-funktio.

## Syventävä sukellus
CSV-tiedostojen käyttö on yleistynyt 1970-luvulta lähtien etenkin liiketoiminnassa ja tietoanalytiikassa. Vaikka Haskelliin on saatavilla monia CSV-käsittelyyn tarkoitettuja kirjastoja, voimme myös käyttää sisäänrakennettuja listafunktioita kuten `map` ja `filter` CSV-tiedoston käsittelyyn.

Vaihtoehtoisesti voimme käyttää myös erilaisia ohjelmistoja ja työkaluja, kuten Exceliä, jotka tarjoavat käyttäjäystävällisen käyttöliittymän CSV-tiedostojen käsittelyyn.

CSV-tiedostojen käsittely perustuu tietojen erottamiseen pilkulla, mutta tilanteesta riippuen voidaan käyttää myös muita erottimia kuten puolipistettä tai välilyöntiä.

## Katso myös
- [Haskellin viralliset dokumentaatiot](https://www.haskell.org/documentation/)
- [Text.CSV-moduulin dokumentaatio](https://hackage.haskell.org/package/csv/docs/Text-CSV.html)