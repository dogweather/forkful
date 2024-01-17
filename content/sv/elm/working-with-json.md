---
title:                "Arbeta med json"
html_title:           "Elm: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Arbetet med JSON handlar om att hantera data i form av textformat. Det är ett vanligt sätt för programmerare att utbyta information och används ofta för att hämta data från en webbserver. Det är ett effektivt sätt att strukturera och organisera data på.

## Hur man gör:

För att kunna jobba med JSON i Elm behöver vi importera biblioteket "Json.Decode". Detta låter oss läsa in JSON-data och omvandla den till Elm-värden. Vi använder sedan funktioner som "decodeString" och "field" för att hämta ut specifikt data från vår JSON-sträng. Nedan följer ett enkelt exempel på hur vi kan läsa in och använda JSON-data:

```Elm
import Json.Decode exposing (..)

-- Enkelt exempel på JSON-data
jsonStrang = """
  {
    "namn": "Maria",
    "ålder": 25,
    "intressen": ["musik", "film", "resor"]
  }
"""

-- Hämta ut namn från JSON-strängen
namn =
  decodeString (field "namn" string) jsonStrang

-- Hämta ut en lista över intressen från JSON-strängen
intressen =
  decodeString (field "intressen" (list string)) jsonStrang

-- Skriv ut resultatet
main =
  Html.text (toString intressen)
```

Resultatet blir då en lista med Marias intressen: `["musik", "film", "resor"]`.

## Djupdykning:

JSON, som står för "JavaScript Object Notation", är en standard för att strukturera och överföra data mellan olika applikationer och system. Det är ett lättläst och kompakt sätt att representera data och har blivit väldigt populärt bland utvecklare eftersom det fungerar i olika programmeringsspråk. Innan JSON användes ofta XML för att strukturera data, men detta var mer komplext och resurskrävande.

Alternativ till JSON är bland annat XML, YAML och CSV. XML är fortfarande ett vanligt sätt att strukturera data, men jämfört med JSON är det mer tungrott och svårläst. YAML är mer lättläst än XML, men mindre välstöd och inte lika standardiserat. CSV är ett vanligt sätt att strukturera tabellformat data, men det har begränsningar när det kommer till mer komplex data.

När vi arbetar med JSON i Elm, så omvandlas den till Elm-värden som sedan kan användas i vårt program. Detta innebär att vi behöver en sträng med giltig JSON-syntax för att kunna behandla datan korrekt. Om vår sträng inte följer rätt syntax kommer det att orsaka fel i vårt program.

## Se också:

- [Json.Decode bibliotekets dokumentation](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [JSON.org](https://www.json.org/json-sv.html) för mer information om JSON-syntax och standarden
- [Elm-guiden](https://guide.elm-lang.org/) för att lära dig mer om Elm och dess funktioner