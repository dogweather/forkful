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

## Vad & Varför?
CSV står för "comma-separated values" och är en vanlig form av filformat som används för att lagra och manipulera data. Programerare använder CSV för att enkelt strukturera, bearbeta och utbyta data mellan olika system och verktyg.

## Hur gör man:
Det finns många sätt att arbeta med CSV i Elm, men ett vanligt tillvägagångssätt är att använda biblioteket csv-decode. För att komma igång behöver du först installera biblioteket genom att köra följande kommando i terminalen:
```Elm
elm install rtfeldman/csv-decode
```
Sedan kan du importera biblioteket i din Elm-kod och använda dess funktioner för att dekodera och bearbeta CSV-data.
```Elm
import Csv.Decode as Decode

myCsv = "Name,Age,Email
Jane,25,jane@email.com
John,30,john@email.com"

type alias Person =
  { name : String
  , age : Int
  , email : String
  }

csvDecoder : Decode.Decoder (List Person)
csvDecoder =
  Decode.decodeString (Decode.list personDecoder)

personDecoder : Decode.Decoder Person
personDecoder =
  Decode.map3 Person
    (Decode.field "Name" Decode.string)
    (Decode.field "Age" Decode.int)
    (Decode.field "Email" Decode.string)
```
För att dekodera CSV-datasträngen "myCsv" till en lista av personer, behöver du bara köra följande kod:
```Elm
Decode.decodeString csvDecoder myCsv
```
Detta kommer att returnera en lista med dekodade personobjekt, som kan användas för att bearbeta och manipulera datan.

## Djupdykning:
CSV-formatet skapades på 1970-talet som en enkel och universell lösning för datautbyte mellan olika system och program. Sedan dess har det blivit ett populärt verktyg för att strukturera och manipulera stora mängder data i olika programeringsspråk.

Det finns också andra alternativ för att arbeta med CSV i Elm, såsom manuell strängmanipulation eller andra tredjepartsbibliotek. Du kan välja att använda det som passar dig bäst beroende på dina specifika behov och preferenser.

Det är också värt att notera att biblioteket csv-decode i dagsläget endast stödjer dekodning av CSV-data, vilket innebär att du behöver skriva egen kod för att kodera eller manipulera datasträngar.

## Se även:
- [Github-repo för biblioteket csv-decode] (https://github.com/rtfeldman/csv-decode)
- [Elm-society - användbar information om Elm] (https://elm-society.gitbook.io/elm-society/)