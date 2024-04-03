---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:16.950022-07:00
description: "Wie: In Haskell k\xF6nnen Sie einen String mit der Standardbibliothek\
  \ gro\xDFschreiben, ohne dass Sie irgendwelche Drittanbieter-Bibliotheken ben\xF6\
  tigen."
lastmod: '2024-03-13T22:44:53.914448-06:00'
model: gpt-4-0125-preview
summary: "In Haskell k\xF6nnen Sie einen String mit der Standardbibliothek gro\xDF\
  schreiben, ohne dass Sie irgendwelche Drittanbieter-Bibliotheken ben\xF6tigen."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
In Haskell können Sie einen String mit der Standardbibliothek großschreiben, ohne dass Sie irgendwelche Drittanbieter-Bibliotheken benötigen.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Beispielverwendung:
main = putStrLn $ capitalize "hello world"
```

Ausgabe:
```
Hello world
```

Für komplexere Szenarien oder zur Vereinfachung der Verwendung möchten Sie möglicherweise eine Drittanbieter-Bibliothek wie `text` verwenden, die für effiziente String-Manipulation in Haskell beliebt ist.

Zuerst müssen Sie `text` zu den Abhängigkeiten Ihres Projekts hinzufügen. Dann können Sie seine Funktionen verwenden, um einen String wie folgt großzuschreiben:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Beispielverwendung mit der Textbibliothek:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Ausgabe:
```
Hello world
```

Beide Beispiele demonstrieren einfache, aber effektive Wege, einen String in Haskell großzuschreiben, mit oder ohne Drittanbieter-Bibliotheken.
