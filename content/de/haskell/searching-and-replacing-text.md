---
title:                "Textsuche und -ersetzung"
html_title:           "Haskell: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Was & Warum?

Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren, bei der spezifische Textsequenzen in einem größeren Text gesucht und durch andere ersetzt werden. Programmierer nutzen diese Funktion häufig, um schnell und effizient wiederkehrende Textelemente zu ändern oder zu aktualisieren.

So geht's:

Eine einfache Möglichkeit, Text in Haskell zu suchen und zu ersetzen, ist die Verwendung der ```substitute``` Funktion aus dem Paket ```Data.String.Utils```. Diese Funktion akzeptiert drei Argumente: den Text, in dem die Ersetzungen durchgeführt werden sollen, den zu suchenden Text und den Ersatztext. Hier ist ein Beispiel:

```Haskell
import Data.String.Utils
let text = "Hallo, ich bin ein Text."
let newText = substitute text "Hallo" "Guten Tag"
```

Das Ergebnis ist der Text ```"Guten Tag, ich bin ein Text."```, da alle Vorkommen des Textes "Hallo" durch "Guten Tag" ersetzt wurden.

Eine andere Möglichkeit ist die Verwendung der Funktion ```replace``` aus dem Paket ```Text.Regex``` in Kombination mit regulären Ausdrücken. Diese Methode ist etwas komplexer, bietet jedoch mehr Flexibilität für komplexere Such- und Ersetzungsmuster. Hier ist ein Beispiel:

```Haskell
import Text.Regex
let text = "Die Sonne scheint im Sommer."
let pattern = mkRegex "Sommer"
let newText = subRegex pattern text "Herbst"
```

Das Ergebnis ist der Text ```"Die Sonne scheint im Herbst."```, da der Text "Sommer" durch "Herbst" ersetzt wurde.

Tiefgehende Einblicke:

Das Suchen und Ersetzen von Text hat eine lange Geschichte in der Informatik, wobei bereits frühe Texteditoren in den 1970er Jahren solche Funktionen boten. Heutzutage ist es eine grundlegende Funktion in den meisten Programmiersprachen und wird von Programmierern auf der ganzen Welt genutzt. Neben den oben genannten Methoden gibt es noch viele weitere Möglichkeiten, Text in Haskell zu durchsuchen und zu ersetzen, wie beispielsweise die Verwendung von regulären Ausdrücken mit dem Paket ```Text.Regex.Posix``` oder die Implementierung eigener Algorithmen.

Siehe auch:

- https://hackage.haskell.org/package/base/docs/Data-String-Utils.html
- https://hackage.haskell.org/package/regex-compat/docs/Text-Regex.html
- https://www.haskell.org/haskellwiki/Regex_Tutorial