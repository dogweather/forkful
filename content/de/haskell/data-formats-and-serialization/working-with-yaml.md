---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:29.490499-07:00
description: "YAML, die Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein\
  \ benutzerfreundlicher Datenserialisierungsstandard, der f\xFCr alle Programmiersprachen\u2026"
lastmod: '2024-02-25T18:49:51.008073-07:00'
model: gpt-4-0125-preview
summary: "YAML, die Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein benutzerfreundlicher\
  \ Datenserialisierungsstandard, der f\xFCr alle Programmiersprachen\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, die Abkürzung für "YAML Ain't Markup Language", ist ein benutzerfreundlicher Datenserialisierungsstandard, der für alle Programmiersprachen verwendet werden kann. Programmierer nutzen YAML oft in Konfigurationsdateien und beim Datenaustausch zwischen Sprachen wegen seiner Lesbarkeit und einfachen Struktur.

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
