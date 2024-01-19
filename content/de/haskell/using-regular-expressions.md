---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (Regular Expressions oder Regex) werden verwendet, um Muster in Strings zu finden und zu manipulieren. Sie sind ungemein nützlich für Textverarbeitungsaufgaben und Datenvalidierung.

## Wie man:

Verwenden Sie in Haskell das Modul `Text.Regex.Posix` für Regex-Operationen. Hier ist ein einfaches Beispiel:

```Haskell
import Text.Regex.Posix

main = putStrLn $ show ("1234" =~ "[0-9]+" :: Bool)
```

Ausführen dieses Codes gibt "`True`" zurück, da der String "`1234`" das Muster "`[0-9]+`" (ein oder mehr Ziffern) entspricht.

## Vertiefung:

Reguläre Ausdrücke haben eine lange Geschichte, die bis in die 1950er Jahre zurückreicht und sind integraler Bestandteil vieler Programmiersprachen. In Haskell gibt es auch Alternativen wie Parsec und Megaparsec, die eine funktionalere Art der Textverarbeitung ermöglichen. Beim Arbeiten mit Regex in Haskell ist es wichtig zu wissen, dass diese Operationen die Unterstützung durch Posix Regex Bibliothek in Betriebssystemebene verwenden.

## Siehe auch:

- Die [Haskell Regex Wiki](http://www.haskell.org/haskellwiki/Regex) für weitere Details und Beispiele.
- Eine vollständige Referenz von Regex-Syntax in [RegexOne](https://regexone.com/).
- Die offizielle Dokumentation für die [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix) Bibliothek.