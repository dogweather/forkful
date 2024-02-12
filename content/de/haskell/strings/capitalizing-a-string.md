---
title:                "Einen String großschreiben"
aliases:
- /de/haskell/capitalizing-a-string/
date:                  2024-02-03T19:05:16.950022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings beinhaltet die Umwandlung des ersten Buchstabens eines gegebenen Strings in einen Großbuchstaben, während sichergestellt wird, dass die restlichen Buchstaben klein bleiben. Programmierer tun dies, um Ausgaben zu formatieren, die Grammatik in Texten einzuhalten oder die Lesbarkeit generierter Daten zu verbessern.

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
