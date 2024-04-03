---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:00.257212-07:00
description: "Wie: Haskell hat keine integrierte Unterst\xFCtzung f\xFCr JSON wie\
  \ JavaScript, aber mit Hilfe von Drittanbieterbibliotheken wie **Aeson** wird die\
  \ Handhabung\u2026"
lastmod: '2024-03-13T22:44:53.952133-06:00'
model: gpt-4-0125-preview
summary: "Haskell hat keine integrierte Unterst\xFCtzung f\xFCr JSON wie JavaScript,\
  \ aber mit Hilfe von Drittanbieterbibliotheken wie **Aeson** wird die Handhabung\
  \ von JSON unkompliziert."
title: Arbeiten mit JSON
weight: 38
---

## Wie:
Haskell hat keine integrierte Unterstützung für JSON wie JavaScript, aber mit Hilfe von Drittanbieterbibliotheken wie **Aeson** wird die Handhabung von JSON unkompliziert. Aeson bietet sowohl High-Level- als auch Low-Level-Funktionen für das Kodieren (Umwandeln von Haskell-Werten in JSON) und Dekodieren (Parsen von JSON in Haskell-Werte).

### Aeson installieren
Fügen Sie zunächst Aeson zu den Abhängigkeiten Ihres Projekts hinzu, indem Sie Ihre `.cabal`-Datei aktualisieren oder Stack oder Cabal direkt verwenden:

```shell
cabal update && cabal install aeson
```
oder, wenn Sie Stack verwenden:
```shell
stack install aeson
```

### JSON parsen
Beginnen wir mit einem grundlegenden Beispiel für das Dekodieren von JSON-Daten in einen Haskell-Typ. Angenommen, wir haben das folgende JSON, das eine Person darstellt:

```json
{
  "name": "John Doe",
  "age": 30
}
```

Definieren Sie zunächst einen entsprechenden Haskell-Datentyp und machen Sie ihn zu einer Instanz von `FromJSON`:

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

-- Funktion, um JSON aus einer Datei zu dekodieren
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Verwendung:
Angenommen, `person.json` enthält die oben gezeigten JSON-Daten, führen Sie aus:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Beispielausgabe:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Haskell-Werte als JSON kodieren
Um einen Haskell-Wert zurück in JSON umzuwandeln, müssen Sie Ihren Typ zu einer Instanz von `ToJSON` machen und dann `encode` verwenden.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Unter der Annahme des Person-Typs von zuvor

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Beispielausgabe:
```json
{"name":"Jane Doe","age":32}
```

Diese Beispiele demonstrieren die Grundlagen der Arbeit mit JSON in Haskell unter Verwendung von Aeson. Denken Sie daran, Aeson bietet viel mehr, einschließlich benutzerdefinierten Parsing-Regeln, Arbeit mit komplex verschachteltem JSON und vieles mehr, geeignet für verschiedene Bedürfnisse und Szenarien.
