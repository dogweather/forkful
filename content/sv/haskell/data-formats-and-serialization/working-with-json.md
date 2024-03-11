---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:57.696831-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) i Haskell inneb\xE4\
  r att tolka JSON-data till Haskell-typer och konvertera Haskell-typer tillbaka till\
  \ JSON.\u2026"
lastmod: '2024-03-11T00:14:11.345331-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) i Haskell inneb\xE4r att\
  \ tolka JSON-data till Haskell-typer och konvertera Haskell-typer tillbaka till\
  \ JSON.\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON (JavaScript Object Notation) i Haskell innebär att tolka JSON-data till Haskell-typer och konvertera Haskell-typer tillbaka till JSON. Programmerare gör detta för att möjliggöra för deras Haskell-applikationer att utbyta data med webbtjänster eller API:er sömlöst, en vanlig praxis i modern mjukvaruutveckling för korsplattforms datadelning.

## Hur:
Haskell har inte inbyggt stöd för JSON som JavaScript, men med hjälp av tredjepartsbibliotek såsom **Aeson**, blir hanteringen av JSON enkel. Aeson erbjuder både hög-nivå och låg-nivå funktioner för kodning (konvertering av Haskell-värden till JSON) och avkodning (tolkning av JSON till Haskell-värden).

### Installation av Aeson
Först, lägg till Aeson i ditt projekts beroenden genom att uppdatera din `.cabal`-fil eller använd Stack eller Cabal direkt:
```shell
cabal update && cabal install aeson
```
eller, om du använder Stack:
```shell
stack install aeson
```

### Tolka JSON
Låt oss börja med ett grundläggande exempel på avkodning av JSON-data till en Haskell-typ. Antag att vi har följande JSON som representerar en person:
```json
{
  "name": "John Doe",
  "age": 30
}
```

Först, definiera en motsvarande Haskell-datayp och gör den till en instans av `FromJSON`:
```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Funktion för att avkoda JSON från en fil
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Användning:
Förutsatt att `person.json` innehåller JSON-datan som visas ovan, kör:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Exempelutdata:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Kodning av Haskell-värden som JSON
För att konvertera ett Haskell-värde tillbaka till JSON behöver du göra din typ till en instans av `ToJSON` och sedan använda `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Förutsatt att person-typen från tidigare

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Exempelutdata:
```json
{"name":"Jane Doe","age":32}
```

Dessa exempel demonstrerar grunderna i att arbeta med JSON i Haskell med hjälp av Aeson. Kom ihåg, Aeson erbjuder mycket mer, inklusive anpassade tolkningsregler, arbete med komplexa inbäddade JSON och mycket mer, lämpligt för olika behov och scenarier.
