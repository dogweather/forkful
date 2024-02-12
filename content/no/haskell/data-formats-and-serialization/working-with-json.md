---
title:                "Arbeider med JSON"
aliases: - /no/haskell/working-with-json.md
date:                  2024-02-03T19:22:52.319734-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med JSON (JavaScript Object Notation) i Haskell innebærer å analysere JSON-data til Haskell-typer og konvertere Haskell-typer tilbake til JSON. Programmerere gjør dette for å muliggjøre at deres Haskell-applikasjoner kan utveksle data med webtjenester eller APIer sømløst, en vanlig praksis i moderne programvareutvikling for kryssplattform datautveksling.

## Hvordan:
Haskell har ikke innebygd støtte for JSON som JavaScript, men med hjelp fra tredjepartsbiblioteker som **Aeson**, blir håndtering av JSON enkelt. Aeson tilbyr både høy- og lavnivåfunksjoner for koding (konvertere Haskell-verdier til JSON) og dekoding (parse JSON til Haskell-verdier).

### Installere Aeson
Først, legg til Aeson i prosjektets avhengigheter ved å oppdatere din `.cabal`-fil eller bruke Stack eller Cabal direkte:

```shell
cabal update && cabal install aeson
```
eller, hvis du bruker Stack:
```shell
stack install aeson
```

### Parse JSON
La oss starte med et grunnleggende eksempel på dekoding av JSON-data til en Haskell-type. Anta at vi har følgende JSON som representerer en person:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Først, definer en tilsvarende Haskell-datatype og gjør den til en instans av `FromJSON`:

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

-- Funksjon for å dekode JSON fra en fil
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Bruk:
Antar at `person.json` inneholder JSON-dataene som vist ovenfor, kjør:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Eksempel på utdata:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Enkoding av Haskell-verdier som JSON
For å konvertere en Haskell-verdi tilbake til JSON, må du gjøre typen din til en instans av `ToJSON` og deretter bruke `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Antatt Person-typen fra før

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Eksempel på utdata:
```json
{"name":"Jane Doe","age":32}
```

Disse eksemplene demonstrerer det grunnleggende i å jobbe med JSON i Haskell ved hjelp av Aeson. Husk, Aeson tilbyr mye mer, inkludert egendefinerte parse-regler, arbeid med komplekse nøstede JSON og mye mer, egnet for ulike behov og scenarioer.
