---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:20.163479-07:00
description: "Wie: Out of the Box bietet Haskell grundlegende Werkzeuge f\xFCr das\
  \ Parsen von Daten an, aber das Nutzen von Bibliotheken wie `time` f\xFCr die\u2026"
lastmod: '2024-03-13T22:44:53.940643-06:00'
model: gpt-4-0125-preview
summary: "Out of the Box bietet Haskell grundlegende Werkzeuge f\xFCr das Parsen von\
  \ Daten an, aber das Nutzen von Bibliotheken wie `time` f\xFCr die Kernfunktionalit\xE4\
  t und `date-parse` oder `time-parse` f\xFCr flexibleres Parsen kann die Aufgabe\
  \ erheblich vereinfachen."
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie:
Out of the Box bietet Haskell grundlegende Werkzeuge für das Parsen von Daten an, aber das Nutzen von Bibliotheken wie `time` für die Kernfunktionalität und `date-parse` oder `time-parse` für flexibleres Parsen kann die Aufgabe erheblich vereinfachen.

Stellen Sie zunächst sicher, dass Sie die `time`-Bibliothek zur Verfügung haben; sie ist oft bei GHC enthalten, aber wenn Sie sie als Abhängigkeit angeben müssen, fügen Sie `time` zur cabal-Datei Ihres Projekts hinzu oder verwenden Sie `cabal install time`, um sie manuell zu installieren.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Verwendung der time-Bibliothek zum Parsen eines Datums in einem Standardformat
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

Beispielverwendung und Ausgabe:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- Ausgabe: Just 2023-03-31 22:00:00 UTC
```

Für komplexere Szenarien, in denen Sie mehrere Formate oder Lokalitäten behandeln müssen, können Bibliotheken von Drittanbietern wie `date-parse` bequemer sein:

Angenommen, Sie haben `date-parse` zu Ihren Abhängigkeiten hinzugefügt und installiert, hier ist, wie Sie es verwenden könnten:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- Das Parsen eines Datumsstrings mit der date-parse-Bibliothek unterstützt mehrere Formate
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

Beispielverwendung mit `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- Ausgabe: Just 2023-04-01
```

Jedes Beispiel demonstriert den grundlegenden Ansatz, einen String in ein nutzbares Datumobjekt in Haskell umzuwandeln. Die Wahl zwischen der Verwendung der integrierten Funktionen der `time`-Bibliothek und der Option für eine Lösung von Drittanbietern wie `date-parse` hängt von den spezifischen Bedürfnissen Ihrer Anwendung ab, wie z.B. dem Spektrum der Eingabeformate, die Sie behandeln müssen.
