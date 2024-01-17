---
title:                "Att arbeta med json"
html_title:           "Haskell: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON (JavaScript Object Notation) är ett enkelt och lättläst sätt att strukturera och dela data. Det har blivit ett populärt format för att utbyta data mellan program och webbapplikationer på grund av sin enkelhet och kompatibilitet med de flesta programmeringsspråk.

## Hur gör man:
För att arbeta med JSON i Haskell kan du använda biblioteket "aeson". Detta gör det möjligt för dig att koda och avkoda JSON-data till Haskell-datastrukturer och vice versa. Här är ett exempel på hur du kan använda det:

```Haskell
import Data.Aeson
import Data.Text

-- Deklarera datatyper för JSON-struktur
data Person = Person { name :: Text, age :: Int }

-- Definiera hur Person-objektet ska avkodas från JSON
instance FromJSON Person where
    parseJSON (Object v) =
        Person <$> v .: "name"
               <*> v .: "age"

-- Skapa ett JSON-objekt
let json = "{\"name\": \"Anna\", \"age\": 25}"

-- Avkoda JSON till en Person
let maybePerson = decode json :: Maybe Person

-- Skriv ut namnet på Person om den blev avkodad framgångsrikt
case maybePerson of
    Nothing -> putStrLn "Kunde inte avkoda JSON"
    Just person -> putStrLn $ "Namn: " ++ (unpack $ name person)
```

Detta kommer att skriva ut "Namn: Anna" på skärmen, eftersom Person-objektet "Anna" är 25 år.

## Djupdykning:
JSON utvecklades ursprungligen av Douglas Crockford och blev först populärt tack vare sin användning i JavaScript. Det finns flera alternativ till "aeson" för att arbeta med JSON i Haskell, t.ex. "json" och "yaml". Det finns också möjlighet att handskas med JSON-data direkt med hjälp av funktioner som "encode" och "decode" från "Data.Aeson".

## Se även:
- [aeson dokumentation](https://hackage.haskell.org/package/aeson)
- [JSON på json.org](https://www.json.org/json-sv.html)