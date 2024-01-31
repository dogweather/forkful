---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei ermöglicht es, Daten persistent zu speichern und später wieder darauf zuzugreifen. Programmierer nutzen es für Log-Dateien, Konfigurationsdaten oder den Austausch von Daten zwischen Programmen.

## Vorgehensweise:

```haskell
-- Importiere das benötigte Modul
import System.IO

-- Funktion, um in eine Datei zu schreiben
schreibeDatei :: FilePath -> String -> IO ()
schreibeDatei pfad inhalt = do
    handle <- openFile pfad WriteMode
    hPutStr handle inhalt
    hClose handle

-- Beispielaufruf
main :: IO ()
main = do
    let pfad = "Beispiel.txt"
    let inhalt = "Hallo, Haskell!"
    schreibeDatei pfad inhalt
```

Ausführung:
```
$ runghc beispiel.hs
```

Beispiel.txt nach Ausführung:
```
Hallo, Haskell!
```

## Deep Dive:

Textdateien werden seit den Anfängen der Computerei genutzt, um Daten zu speichern. Abseits von `openFile`, `hPutStr` und `hClose` gibt es in Haskell andere Methoden wie `writeFile` – ein einfacher, aber weniger flexibler Ansatz. Die Kontrolle über den Filehandle ermöglicht feinere Operationen wie Pufferung und Fehlerbehandlung.

## Siehe Auch:

- Haskell IO Tutorial: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
- System.IO Dokumentation: [https://hackage.haskell.org/package/base/docs/System-IO.html](https://hackage.haskell.org/package/base/docs/System-IO.html)
