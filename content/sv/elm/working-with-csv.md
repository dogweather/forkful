---
title:                "Elm: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med stora mängder data, är du förmodligen bekant med CSV-filer. Dessa filer tillåter dig att lägga ut data i en tabellstruktur, vilket gör det enklare att organisera och analysera. Men varför ska du använda Elm för att arbeta med CSV-filer? Fortsätt läsa för att ta reda på det.

## Så här gör du

För att arbeta med CSV i Elm, behöver du först importera paketet `elm-csv` i din kod. Sedan kan du använda funktionen `parse` för att läsa in din CSV-fil och konvertera den till en Elm-modell. Här är ett enkelt exempel:

```Elm
import Csv exposing (..)

type alias Person =
  { name : String
  , age : Int
  }

csvData : String
csvData = 
  "name,age
  John,30
  Jane,25
  Bob,40"

main : List Person
main = 
  case parse csvData of
    Ok people ->
      people
    Err error ->
      Debug.crash error
```

Genom att köra `main` funktionen i din Elm REPL, kommer du att få en lista med `Person` objekt som representerar varje rad i din CSV-fil.

## Djupdykning

När du arbetar med CSV-filer i Elm, finns det några användbara funktioner som du kan använda dig av. Till exempel, om du vill välja specifika kolumner från din CSV, kan du använda `fields` funktionen för att välja dessa och konvertera dem till `String` eller `Int`. Om du vill filtrera bort vissa rader baserat på ett visst villkor, kan du använda `filter` funktionen.

Det viktigaste att komma ihåg när du arbetar med CSV-filer i Elm är att det är ett starkt typat språk. Detta innebär att du måste definiera en specifik modell för din CSV-data och sedan matcha denna modell när du läser in och bearbetar filen. Detta ger dig en säkrare och mer strukturerad process för att hantera data.

## Se även

- Elm CSV paket dokumentation: https://package.elm-lang.org/packages/elm-explorations/csv/latest/
- Elm REPL: https://elm-lang.org/try