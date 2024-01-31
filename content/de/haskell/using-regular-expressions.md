---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, um Text nach bestimmten Regeln zu durchsuchen und zu bearbeiten. Programmierer nutzen sie, weil sie leistungsstark sind und komplexe Textoperationen vereinfachen.

## How to:
In Haskell verwenden wir das `regex` Paket, um mit regulären Ausdrücken zu arbeiten. Installiere es mit `cabal install regex-posix`. Hier sind einige Beispiele:

```Haskell
import Text.Regex.Posix

-- Überprüfung, ob ein Text einem Muster entspricht
"hello world" =~ "hello" :: Bool
-- True

-- Ersetze alle Instanzen von "world" mit "Haskell"
"hello world" =~ "world" :: String -> "hello Haskell"
-- "hello Haskell"

-- Extrahiere Zahlen aus einem String
"Kaufe 3 Äpfel und 5 Bananen" =~ "[0-9]+" :: [[String]]
-- [["3"], ["5"]]
```

## Deep Dive
Reguläre Ausdrücke sind nicht neu. Sie entstanden in den 1950er Jahren und sind in vielen Programmiersprachen eingebettet. Alternativen zu regulären Ausdrücken sind Stringverarbeitungsfunktionen oder Parser-Kombinatoren, beispielsweise das Paket `parsec` in Haskell. Bei der Implementierung wird ein regulärer Ausdruck häufig in einen Zustandsautomaten übersetzt, der effizient Text verarbeiten kann.

## See Also
- Haskell `regex-posix` Paket: https://hackage.haskell.org/package/regex-posix
- Haskell Wiki über reguläre Ausdrücke: https://wiki.haskell.org/Regular_expressions
- `parsec` Parsing-Bibliothek: https://hackage.haskell.org/package/parsec
