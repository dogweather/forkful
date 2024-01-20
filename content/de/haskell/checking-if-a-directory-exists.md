---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:44.812703-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Überprüfen, ob ein Ordner existiert, bedeutet, sicherzustellen, dass ein Pfad auf ein tatsächlich vorhandenes Verzeichnis zeigt. Programmierer machen das, um Fehler zu vermeiden, die entstehen, wenn ihr Code mit nicht vorhandenen Dateisystempfaden arbeiten soll.

## How to:
Hier ist ein einfaches Beispiel, wie du in Haskell mit der Bibliothek `directory` überprüfen kannst, ob ein Verzeichnis existiert:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let path = "/path/to/directory"
  directoryExists <- doesDirectoryExist path
  putStrLn $ "Existiert das Verzeichnis? " ++ show directoryExists
```

Wenn das Verzeichnis existiert, wird die Ausgabe `True` sein, andernfalls `False`.

## Deep Dive
Haskell bietet über seine Paketbibliothek mehrere Wege, die Existenz von Verzeichnissen zu prüfen. Das `directory`-Paket ist der Standardweg, weil es direkt die plattformabhängigen Systemaufrufe kapselt. Historisch gesehen entstammen solche Funktionen dem UNIX-Konzept der Dateisystemverwaltung. Alternativen wie `System.FilePath` oder `unix`-Paket bieten ähnliche Funktionalitäten, manchmal mit zusätzlichen Features. Bei der Umsetzung wird tatsächlich oft die POSIX-Funktion `stat` verwendet, um Informationen über Dateipfade zu erhalten.

## See Also
- Haskell `directory` paket: [https://hackage.haskell.org/package/directory](https://hackage.haskell.org/package/directory)
- Haskell IO Tutorial mit weiteren Dateisystemoperationen: [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
- POSIX `stat` Funktion: [https://linux.die.net/man/2/stat](https://linux.die.net/man/2/stat)