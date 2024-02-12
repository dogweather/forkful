---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:22:32.011377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit TOML beinhaltet das Parsen und Generieren von TOML-Daten (Tom's Offensichtliche, Minimale Sprache) mit Haskell. Programmierer tun dies, um Konfigurationsdateien leicht zu verwalten oder Datenaustausch mit starken Typgarantien und minimalen Syntaxproblemen durchzuführen.

## Wie geht das:
Zuerst, stellen Sie sicher, dass Sie eine TOML-Parsing-Bibliothek haben. Für Haskell ist `htoml` eine beliebte Wahl. Sie müssen es zu den Abhängigkeiten Ihres Projekts hinzufügen.

```Haskell
-- Importieren der TOML-Parsing-Bibliothek
import qualified Text.Toml as Toml

-- Definieren Ihrer Konfigurations-Datenstruktur
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- Optionales Datum
} deriving (Show)

-- Parsen eines TOML-Strings
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Fehler: " ++ show err
    Right toml -> print toml -- Oder weiterverarbeiten des geparsten TOML
```

Beispielausgaben können strukturiert und wie jeder Haskell-Datentyp abgerufen werden.

## Tiefere Einblicke
Historisch wurde TOML von Tom Preston-Werner, dem Mitbegründer von GitHub, als Reaktion auf die Komplexitäten von YAML und JSON für Konfigurationsdateien geschaffen. Es betont, lesbarer und einfacher zu schreiben zu sein als JSON und strenger sowie einfacher als YAML.

Alternativen zu TOML beinhalten JSON und YAML, wobei jedes Format seine eigenen Stärken hat. JSON ist allgegenwärtig und sprachenunabhängig, während YAML ein menschenlesbareres Format bietet. TOML wird für seine Einfachheit und Konsistenz geschätzt und vermeidet einige der Fallstricke seiner Verwandten.

Die Implementierung in Haskell beinhaltet typischerweise eine Bibliothek, die TOML in einen Haskell-Datentyp parst, und nutzt oft das fortgeschrittene Typsystem von Haskell, um Korrektheit zu gewährleisten. Das Parsen kann durch rekursiven Abstieg oder Kombinator-Parsing erfolgen, was Effizienz mit Lesbarkeit und Wartbarkeit des Codes ausbalanciert.

## Siehe auch
- `htoml`: https://hackage.haskell.org/package/htoml
- Offizielles TOML GitHub-Repository: https://github.com/toml-lang/toml
- Vergleich von Daten-Serialisierungsformaten: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
