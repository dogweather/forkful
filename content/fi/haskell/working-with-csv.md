---
title:                "Haskell: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi työskennellä CSV-tiedostojen kanssa?

CSV (Comma-Separated Values) on yleisesti käytetty tiedostomuoto datan tallentamiseen ja jakamiseen. Monet ohjelmistot ja työkalut tukevat CSV-muotoa, joten se on erinomainen valinta käsittelemään tietoja useiden eri ohjelmistojen välillä. Lisäksi CSV-tiedostoja on helppo lukea ja muokata, mikä tekee niistä suositun ratkaisun datan käsittelyyn.

## Kuinka työskennellä CSV-tiedostojen kanssa?

CSV-tiedostojen käsittely Haskellilla on yksinkertaista, sillä on olemassa hyödyllisiä kirjastoja, kuten [csv](https://hackage.haskell.org/package/csv) ja [cassava](https://hackage.haskell.org/package/cassava), joilla voit helposti lukea ja kirjoittaa CSV-tiedostoja. Esimerkiksi, jos haluat lukea CSV-tiedoston nimeltä "data.csv" ja tulostaa sen sisällön, voit tehdä sen seuraavasti:

```Haskell
import Text.CSV (parseCSV, Record)

main :: IO ()
main = do
    csvData <- parseCSVFromFile "data.csv"
    either handleError print csvData

handleError :: String -> IO ()
handleError err = putStrLn $ "Virhe: " ++ err    

print :: Either String [Record] -> IO ()
print (Right records) = mapM_ (putStrLn . show) records
print (Left err) = print err
```

Kun ajat tämän koodin, näet CSV-tiedoston sisällön tulostettuna näytölle.

## Syvempi sukellus CSV-tiedostojen työstämiseen

CSV-tiedostojen työstämisessä on hyvä huomioida, että tiedostojen väliset erot voivat vaihdella eri käyttöjärjestelmissä. Esimerkiksi Windows-käyttöjärjestelmässä rivinvaihdot merkitään kahdella merkillä, kun taas Linux-käyttöjärjestelmässä vain yhdellä. Tämän vuoksi CSV-tiedostoja luettaessa kannattaa käyttää [csv-parse](https://hackage.haskell.org/package/csv-parse) tai [cassava-parse](https://hackage.haskell.org/package/cassava) -funktioita, joilla voi määrittää oikean rivinvaihdon merkin. Lisäksi näiden kirjastojen avulla voit helposti muuntaa CSV-tiedostosta luetun datan haluttuun muotoon.

## Katso myös

- [csv - Hackage](https://hackage.haskell.org/package/csv)
- [cassava - Hackage](https://hackage.haskell.org/package/cassava)
- [csv-parse - Hackage](https://hackage.haskell.org/package/csv-parse)
- [cassava-parse - Hackage](https://hackage.haskell.org/package/cassava-parse)