---
date: 2024-01-20 17:42:33.531069-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet,\
  \ bestimmte Zeichen aus einem String zu entfernen, die einem vorgegebenen Kriterium\u2026"
lastmod: '2024-03-13T22:44:53.915441-06:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte\
  \ Zeichen aus einem String zu entfernen, die einem vorgegebenen Kriterium entsprechen."
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

## Anleitung:
Um Zeichen zu löschen, die einem Muster in Haskell entsprechen, können wir die `filter`-Funktion zusammen mit einer passenden Bedingung verwenden. Hier ein einfaches Beispiel, das alle Ziffern aus einem String entfernt:

```Haskell
import Data.Char (isDigit)

deleteDigits :: String -> String
deleteDigits = filter (not . isDigit)

main :: IO ()
main = putStrLn $ deleteDigits "Haskell 2023 ist cool"
```

Ausgabe:

```
Haskell  ist cool
```

Für kompliziertere Muster können wir den `Text.Regex`-Modul verwenden:

```Haskell
import Text.Regex (mkRegex, subRegex)

deletePattern :: String -> String -> String
deletePattern pattern input = subRegex (mkRegex pattern) input ""

main :: IO ()
main = putStrLn $ deletePattern "[0-9]+" "Haskell 2023 ist cool"
```

Ausgabe:

```
Haskell  ist cool
```

## Tiefergehende Informationen:
Das Löschen von Mustern in Strings hat in der Geschichte der Programmierung eine lange Tradition und ist besonders in der Textverarbeitung und beim Parsing von Daten wichtig. In Haskell, wie in vielen anderen funktionalen Sprachen, bieten Funktionen wie `filter` und Pakete wie `regex-base` eine mächtige und expressive Weise, diese Operationen durchzuführen. Alternativ zu `regex-base` gibt es auch `regex-pcre`, die Perl-ähnliche Reguläre Ausdrücke unterstützt, was für manche Entwickler vertrauter ist.

Im Kern ist die Implementierung dieser Funktionalität in Haskell daran interessiert, Seiteneffekte zu vermeiden und Immutable (unveränderliche) Datenstrukturen zu verwenden, was zu einem prägnanten und sicheren Code führt.

## Siehe auch:
- Haskell `Text.Regex` Dokumentation: https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html
- `regex` GitHub Repository: https://github.com/haskell-hvr/regex
- Haskell Wiki zu Regulären Ausdrücken: https://wiki.haskell.org/Regular_expressions
