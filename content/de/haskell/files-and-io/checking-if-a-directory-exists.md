---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:30.368099-07:00
description: "Wie: Haskell bietet durch seine Basisbibliothek einfache M\xF6glichkeiten,\
  \ die Existenz eines Verzeichnisses zu \xFCberpr\xFCfen, haupts\xE4chlich mit dem\
  \ Modul\u2026"
lastmod: '2024-03-13T22:44:53.945354-06:00'
model: gpt-4-0125-preview
summary: "Haskell bietet durch seine Basisbibliothek einfache M\xF6glichkeiten, die\
  \ Existenz eines Verzeichnisses zu \xFCberpr\xFCfen, haupts\xE4chlich mit dem Modul\
  \ `System.Directory`."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Wie:
Haskell bietet durch seine Basisbibliothek einfache Möglichkeiten, die Existenz eines Verzeichnisses zu überprüfen, hauptsächlich mit dem Modul `System.Directory`. Schauen wir uns ein einfaches Beispiel an:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Does the directory exist? " ++ show exists
```

Beispielausgabe, abhängig davon, ob das Verzeichnis existiert:

```
Exists the directory exist? True
```
Oder:
```
Exists the directory exist? False
```

Für komplexere Szenarien oder zusätzliche Funktionalitäten könnten Sie eine beliebte Drittanbieterbibliothek wie `filepath` in Betracht ziehen, um Dateipfade auf eine abstraktere Weise zu handhaben und zu manipulieren. Allerdings ist für den Zweck, einfach zu überprüfen, ob ein Verzeichnis existiert, das Modul `System.Directory` der Basisbibliothek ausreichend und effizient.

Denken Sie daran, dass die Arbeit mit Dateisystemen plattformübergreifend variieren kann und dass Haskells Ansatz versucht, einige dieser Unterschiede zu abstrahieren. Testen Sie immer Ihre Dateioperationen auf dem Zielsystem, um das erwartete Verhalten sicherzustellen.
