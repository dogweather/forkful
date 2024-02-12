---
title:                "Erstellung einer temporären Datei"
aliases:
- /de/haskell/creating-a-temporary-file/
date:                  2024-01-20T17:40:32.770787-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Temporäre Dateien sind kurzlebige Dateien für Daten, die während der Laufzeit eines Programms benötigt, aber nicht dauerhaft gespeichert werden sollen. Programmierer nutzen sie, um Speicherplatz zu sparen, die Sicherheit zu erhöhen und Race Conditions zu vermeiden.

## So geht's:
Mit der Haskell-Bibliothek `temporary` kann man leicht temporäre Dateien erstellen und verwalten. Hier ein Beispiel:

```haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents)

main :: IO ()
main = withSystemTempFile "meineTempDatei.txt" $ \tempFile hFile -> do
  -- Schreibe etwas in die temporäre Datei
  hPutStrLn hFile "Das ist ein Test."

  -- Lies den Inhalt der temporären Datei
  hSeek hFile AbsoluteSeek 0  -- Zurück zum Anfang der Datei
  content <- hGetContents hFile
  putStrLn $ "Die temporäre Datei enthält: " ++ content

-- Ausgabe: "Die temporäre Datei enthält: Das ist ein Test."
```

## Tiefgang:
Historisch gesehen wurden temporäre Dateien oft manuell mit spezifischen Dateinamen erstellt, was zu Sicherheitsrisiken führte. Moderne Ansätze, wie die `temporary`-Bibliothek, vermeiden diese Probleme indem sie automatisch eindeutige Namen generieren. Alternativ zur temporären Datei kann man auch In-Memory-Strukturen nutzen, wenn die Performance kritisch ist. Bei der Implementation ist wichtig, dass das Betriebssystem die Datei löscht, auch wenn das Programm unerwartet beendet wird.

## Siehe auch:
- Haskell `temporary` Dokumentation: https://hackage.haskell.org/package/temporary
- Haskell bytestring für effiziente Datei-Operationen: https://hackage.haskell.org/package/bytestring
- Haskell System.IO Dokumentation: https://hackage.haskell.org/package/base/docs/System-IO.html
