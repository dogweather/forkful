---
title:                "Arbeta med csv"
html_title:           "Elm: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför 

Att arbeta med CSV-filer kan vara en användbar färdighet för många olika yrken och projekt. Med Elm som språk är det enkelt och effektivt att hantera stora mängder data och utföra olika operationer på det.

## Hur man gör

För att arbeta med CSV-filer i Elm behöver du först importera modulen `Csv.Decode` och öppna en CSV-fil i ett `Csv.Document`-objekt. Sedan kan du använda funktionen `Decode.csv` för att tolka filen och göra om den till en Elm-lista. 

```Elm
import Csv.Decode exposing (document, csv)
import Html exposing (..)

view : Model -> Html Msg
view model =
  let
    file = "data.csv"
    csvDoc = Csv.Decode.document file

    rows = case csvDoc of
      Ok doc ->
        Parse.csv doc |> Expect.equal [ ["1","John","Doe"]
                                       ["2","Jane","Smith"]
                                       ["3","Bob","Johnson"]
                                     ]

      Err err ->
        Debug.crash err
  in
  div []
    [ h1 [] [ text "CSV Hantering" ]
    , rows -- Ersätt med din egen kod för att rendera datan
    ]

```
## Djupdykning

Att arbeta med CSV-filer i Elm kan även inkludera att välja specifika kolumner, filtrera rader baserat på villkor och skapa nya filer baserat på det bearbetade resultatet. Det finns även en rad andra moduler som kan vara användbara när man hanterar CSV-filer i Elm, som `Csv.Encode` för att spara data till en CSV-fil och `Csv.Decode.Mappers` för att anpassa sin dekoder efter ens specifika databehov. 

## Se även

- [Elm Dokumentation](https://guide.elm-lang.org/)
- [CSV Hantering i Elm Guide](https://elmprogramming.com/csv-file-processing-in-elm.html)