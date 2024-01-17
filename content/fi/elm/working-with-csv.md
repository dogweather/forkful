---
title:                "Työskentely csv:n kanssa"
html_title:           "Elm: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
CSV:n käsittely on yleinen ohjelmoinnissa, sillä se on helppo ja tehokas tapa tallentaa tietoja taulukkomuodossa. CSV (comma-separated values) on tiedostomuoto, jossa tiedot erotellaan pilkulla. CSV-tiedostoja käytetään usein tietokantojen tai Excel-taulukoiden tuontiin ja vientiin.

## Miten tehdä:
```Elm
import Csv

-- Avaamme CSV-tiedoston ja tallennamme sen sisällön listaan
Csv.decodeFile "tiedostonimi.csv" |> Task.perform
  (\either ->
    case either of
      Err error -> Debug.log "Virhe!" error
      Ok data -> Debug.log "Data:" data
  )
```

CSV-tiedoston avaamiseen ja sisällön listaan tallentamiseen tarvitaan Csv-kirjasto eli paketti. Tämän jälkeen tiedoston nimi annetaan ```decodeFile``` -funktiolle, joka palauttaa joko virheen tai oikean datan.

## Syvemmälle:
CSV:llä on pitkä historia, ja se on peräisin jo 1970-luvulta. Siinä käytetyt erotinmerkit vaihtelevat eri maissa, sillä esimerkiksi Yhdysvalloissa käytetään pilkkua desimaalierottimena. Alternatiivina CSV-tiedostoille on JSON muodossa tallennetut tiedostot, mutta CSV on edelleen suosittu erityisesti taulukkomuotoisen datan käsittelyssä.

## Lue myös:
- [Elmin virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Csv-kirjasto](https://package.elm-lang.org/packages/elm-explorations/csv/latest/Csv)