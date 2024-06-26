---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:29.490499-07:00
description: "Wie zu: Haskell hat keine integrierte Unterst\xFCtzung f\xFCr die Verarbeitung\
  \ von YAML, aber Sie k\xF6nnen Drittanbieter-Bibliotheken wie `yaml` und `aeson`\
  \ f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.951132-06:00'
model: gpt-4-0125-preview
summary: "Haskell hat keine integrierte Unterst\xFCtzung f\xFCr die Verarbeitung von\
  \ YAML, aber Sie k\xF6nnen Drittanbieter-Bibliotheken wie `yaml` und `aeson` f\xFC\
  r das Parsen und Generieren von YAML-Daten verwenden."
title: Arbeiten mit YAML
weight: 41
---

## Wie zu:
Haskell hat keine integrierte Unterstützung für die Verarbeitung von YAML, aber Sie können Drittanbieter-Bibliotheken wie `yaml` und `aeson` für das Parsen und Generieren von YAML-Daten verwenden. Hier ist, wie Sie beginnen können:

### YAML lesen
Fügen Sie zunächst das `yaml`-Paket zu den Abhängigkeiten Ihres Projekts hinzu. Dann können Sie das folgende Beispiel verwenden, um ein einfaches YAML-Dokument zu parsen:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Beispiel YAML-Daten
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Definieren Sie eine Datenstruktur, die dem YAML-Dokument entspricht
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Fehler beim Parsen von YAML: " ++ show err
    Right person -> print person
```
Die Ausgabe des obigen Codes könnte wie folgt aussehen:
```
Person {name = "John Doe", age = 30}
```

### YAML schreiben
Um YAML aus Haskell-Datenstrukturen zu generieren, können Sie die Kodierungsfunktionalitäten des `yaml`-Pakets wie unten gezeigt verwenden:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Unter Verwendung der Person-Datenstruktur aus dem vorherigen Beispiel

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
Die Ausgabe dieses Programms wird ein YAML-formatierter String sein:
```
name: Jane Doe
age: 25
```

Diese Beispiele sollten als Ausgangspunkt für die Arbeit mit YAML in Haskell dienen. Je nach Ihren Bedürfnissen möchten Sie vielleicht fortgeschrittene Funktionen und Optionen erkunden, die diese Bibliotheken bieten.
