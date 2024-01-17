---
title:                "Eine Zeichenfolge interpolieren"
html_title:           "Haskell: Eine Zeichenfolge interpolieren"
simple_title:         "Eine Zeichenfolge interpolieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Was ist das & Warum?
Interpolation ist eine Technik, die es Programmierern ermöglicht, Variablen und Ausdrücke in einer Zeichenkette einzufügen. Dies kann hilfreich sein, um dynamische oder flexible Texte zu erstellen, beispielsweise in einer Benutzeroberfläche oder einer Website.

# Wie geht's?
```Haskell
-- Beispiel einer Zeichenkette ohne Interpolation
let name = "Max"
let age = 25
let info = "Mein Name ist " ++ name ++ " und ich bin " ++ show age ++ " Jahre alt."

-- Ergebnis: "Mein Name ist Max und ich bin 25 Jahre alt."

-- Mit Interpolation sieht der Code so aus:
let name = "Max"
let age = 25
let info = "Mein Name ist #{name} und ich bin #{age} Jahre alt."

-- Ergebnis: "Mein Name ist Max und ich bin 25 Jahre alt."
```
Wie Sie sehen, können Variablen und Ausdrücke in der Zeichenkette mit `#{...}` eingefügt werden, anstatt diese mit `++` zu verketten.

# Tiefer tauchen
- Historischer Kontext: Interpolation wurde erstmals in der Programmiersprache Lisp eingeführt und hat sich seitdem zu einem gängigen Konzept in vielen Programmiersprachen entwickelt.
- Alternativen: In einigen Sprachen, wie z.B. Python, gibt es die Funktion `format()`, die ähnliche Ergebnisse erzielt. Auch in Haskell gibt es einige Bibliotheken, die speziell für die Interpolation von Zeichenketten ausgelegt sind.
- Implementierungsdetails: In Haskell wird die `show` Funktion verwendet, um Variablen in Zeichenketten umzuwandeln, während in anderen Sprachen die `to_s` oder `str()` Funktionen verwendet werden.

# Sieh' auch
- [Haskell-Dokumentation zur Verkettung von Zeichenketten](https://haskell.org/ghc/docs/latest/html/libraries/base-4.14.1.0/GHC-Base.html#v:-43-)
- [Python-Dokumentation zur String-Formatierung](https://docs.python.org/3.7/library/stdtypes.html#str.format)
- [Lisp-Referenz zur Interpolation von Zeichenketten](https://lisp.org/HyperSpec/Body/fun_format.html)