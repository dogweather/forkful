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

## Miksi

Elm on nykyään yksi suosituimmista ohjelmointikielistä, ja sen yksi vahvuus on sen vaivaton integroitavuus muihin tiedostomuotoihin, kuten CSV-tiedostoihin. CSV-tiedostot ovat yleisiä ja niihin tallennetaan usein suuria määriä tietoa, joten kyky työskennellä niiden kanssa on hyödyllinen taito ohjelmoijalle.

## Miten

Elm tarjoaa meille vahvan CSV-paketin, joka helpottaa CSV-tiedostojen lukemista ja kirjoittamista. Aloittaaksesi, sinun tulee ensin asentaa elm-csv-paketti projektisi riippuvuuksiksi. Voit tehdä tämän ajamalla seuraavan komennon komentokehotteessa:

```
elm install arowM/elm-csv
```

Sitten voit aloittaa työskentelyn seuraavalla yksinkertaisella koodilla, joka lukee CSV-tiedoston ja tallentaa sen listaksi:

```
import Csv

Csv.decode false "," "sample.csv"
    |> Result.toMaybe
    |> List.map (\row -> row.member)
```

Tässä esimerkissä käytämme `Csv.decode` -funktiota, joka ottaa parametreina delimiterin, joka erottaa tiedostossa sarakkeet, sekä tiedoston nimen.

Voit myös halutessasi kirjoittaa tietoja CSV-tiedostoon käyttäen `Csv.Encode` -funktiota. Esimerkiksi:

```
import Csv

sampleData =
  [ [ "Nimi", "Ikä", "Harrastukset" ]
  , [ "Leena", "27", "Maalaaminen, valokuvaus" ]
  , [ "Matti", "31", "Tennis, kirjoittaminen" ]
  ]

Csv.encode sampleData
    |> List.map (Csv.toText ",")
    |> List.concat |> String.join "\n"
    |> File.write "sample.csv"
```

## Syvällinen sukellus

Elm tarjoaa meille myös mahdollisuuden käsitellä CSV-tiedostojen käsittelyä virheiden varalta. Voit lisätä `Csv` -pakettiin parametrin `True`, joka ilmoittaa, että tiedostosta on odotettavissa virheitä. Tämä auttaa varmistamaan, että ohjelmasi toimii luotettavasti myös silloin, jos tiedostossa on virheitä.

Voit myös lisätä CSV-tiedostojen muokkaamiseen muita toimintoja, kuten sarakkeiden lisäämistä tai poistamista, `List` - ja `Maybe` -funktioita käyttäen. Voit löytää lisää tietoa elm-csv-paketin dokumentaatiosta.

## Katso myös

- [elm-csv-paketin dokumentaatio](https://package.elm-lang.org/packages/arowM/elm-csv/latest/)
- [Virallinen Elm-sivusto](https://elm-lang.org/)
- [Elm-yhteisön foorumi](https://discourse.elm-lang.org/)