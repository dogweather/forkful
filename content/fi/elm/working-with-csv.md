---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV on yksinkertainen tiedostomuoto, jota käytetään tiedon tallentamiseen ja vaihtamiseen. Ohjelmoijat käyttävät sitä datan helpon siirrettävyyden ja laajan sovellustuen vuoksi.

## How to:
Elmissä CSV-tiedoston käsittely vaatii pientä kikkailua, koska puhtaita stringioperaatioita suositaan.

```Elm
import Html exposing (Html, text)
import String

type alias CSV =
  List (List String)

parseCSV : String -> CSV
parseCSV input =
  input
    |> String.split "\n"
    |> List.map (String.split ",")

viewCSV : CSV -> Html msg
viewCSV csvData =
  text (String.join "\n" (List.map (String.join ", ") csvData))

main =
  viewCSV (parseCSV "name,age\nAlice,30\nBob,25")
```

Tulostaa:
```
name,age
Alice,30
Bob,25
```

## Deep Dive:
CSV (Comma-Separated Values) on ollut käytössä jo vuosikymmeniä. Suosio perustuu yksinkertaisuuteen ja lukuisten ohjelmien tukiin. Elm-tapauksessa käyttämättömyys johtuu sen funktionaalisen paradigman ja selkeän tyypin turvallisuuden korostamisesta. CSV-kirjastoja on saatavilla, mutta vakio syntaksi puuttuu. Käsittely on vaivatonta perusoperaatioita käyttäen, mutta monimutkaisempi datan validointi vaatii lisäkirjastoja tai itse kirjoitettua koodia.

## See Also:
- Elm ja HTTP-datan käsittely: [Working with HTTP in Elm](https://guide.elm-lang.org/effects/http.html)
- String-moduulin dokumentaatio: [Elm String Module](https://package.elm-lang.org/packages/elm/core/latest/String)
