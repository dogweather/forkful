---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:16.950022-07:00
description: "Das Gro\xDFschreiben eines Strings beinhaltet die Umwandlung des ersten\
  \ Buchstabens eines gegebenen Strings in einen Gro\xDFbuchstaben, w\xE4hrend sichergestellt\u2026"
lastmod: 2024-02-19 22:05:12.841903
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings beinhaltet die Umwandlung des ersten\
  \ Buchstabens eines gegebenen Strings in einen Gro\xDFbuchstaben, w\xE4hrend sichergestellt\u2026"
title: "Einen String gro\xDFschreiben"
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
