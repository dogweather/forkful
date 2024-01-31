---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML, "YAML Ain't Markup Language", ist ein Format zum Speichern von Konfigurationsdaten. Programmierer verwenden es wegen seiner einfachen Les- und Schreibbarkeit. Es ist besonders beliebt für Konfigurationsdateien in Softwareprojekten und, durch seine menschenlesbare Struktur, als Austauschformat für Daten zwischen verschiedenen Programmiersprachen.

## How to:
Um YAML in Haskell zu bearbeiten, nutzen wir die `yaml` Bibliothek. Installiere sie erst mit `stack install yaml` oder `cabal install yaml`.

```Haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
  yamlData <- BS.readFile "config.yaml"
  let parsedResult = decodeEither' yamlData :: Either ParseException Value
  case parsedResult of
    Left ex -> putStrLn $ "Fehler beim Parsen der YAML-Daten: " ++ show ex
    Right val -> print val
```

Angenommen `config.yaml` sieht so aus:

```
name: Max Mustermann
age: 30
```

Das Resultat wäre:

```
Object fromList [("name", String "Max Mustermann"), ("age", Number 30.0)]
```

## Deep Dive:
YAML wurde 2001 eingeführt und hat im Laufe der Jahre wegen seiner Einfachheit in der Nutzung an Beliebtheit gewonnen. Als Alternative bieten sich JSON oder XML an, die stärker strukturiert sind, aber YAMLs Vorteil ist seine Nähe zur menschlichen Sprache. In Haskell wird YAML mittels des `yaml` Pakets geparst, das letztlich LibYAML, eine C-Bibliothek, umschließt. So profitieren Haskell-Programmierer von der Schnelligkeit und Effizienz dieser erwiesenen Bibliothek.

## See Also:
Weitere Details finden Sie in der Dokumentation:

- YAML-Spezifikation: https://yaml.org/spec/1.2/spec.html
- Haskell `yaml` Paket: https://hackage.haskell.org/package/yaml
- Zum Vergleich – JSON in Haskell mit `aeson`: https://hackage.haskell.org/package/aeson

Bitte beachte, dass diese Links in englischer Sprache sind.
