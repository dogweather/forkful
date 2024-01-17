---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Haskell: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug, das Programmierern hilft, Texte auf eine präzise und flexible Weise zu durchsuchen und zu manipulieren. Sie bestehen aus speziellen Zeichenfolgen, die eine Mustererkennung ermöglichen. Programmierer verwenden reguläre Ausdrücke in verschiedenen Sprachen, einschließlich Haskell, um komplexe Aufgaben wie das Validieren von Benutzereingaben oder das Extrahieren von Daten aus Texten zu lösen.

## Wie geht's?
Reguläre Ausdrücke werden in Haskell mithilfe des Pakets "regex" verwendet. Zunächst muss das Paket importiert werden:
```Haskell
import Text.Regex
```
Dann können wir eine reguläre Ausdrucksstruktur definieren, indem wir den Typ "Regex" verwenden:
```Haskell
myRegex :: Regex
```
Um einen regulären Ausdruck in Aktion zu sehen, können wir ihn in der Funktion "matchRegex" verwenden:
```Haskell
> matchRegex (mkRegex "([0-9]+)") "abc123def"
Just ["123"] 
```
In diesem Beispiel wird der reguläre Ausdruck "([0-9]+)" verwendet, um nach Zahlen in einem String zu suchen und sie als Ergebnis zurückzugeben.

## Tiefer tauchen
Reguläre Ausdrücke wurden bereits in den 1950er Jahren von dem Mathematiker Stephen Kleene entwickelt und sind seitdem ein wichtiger Bestandteil der Computerwissenschaften. Neben Haskell gibt es auch in anderen Programmiersprachen, wie z.B. Perl und Java, eine Unterstützung für reguläre Ausdrücke.

Für Programmierer, die mit der Syntax von regulären Ausdrücken nicht vertraut sind, kann es schwierig sein, komplexe Ausdrücke zu schreiben. Eine alternative Möglichkeit, Text zu durchsuchen und zu manipulieren, ist die Verwendung von Bibliotheken wie "stringsearch", die eine höhere Abstraktionsebene bieten.

In Haskell wird das Regex-Paket durch die Funktionen "mkRegex", "matchRegex" und "splitRegex" implementiert, die über das Array Library Framework (ALF) bereitgestellt werden.

## Siehe auch
- [Offizielle Dokumentation des Regex-Pakets in Haskell](https://hackage.haskell.org/package/regex)
- [Ein Tutorial zur Verwendung von regulären Ausdrücken in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/regular-expressions)
- [Ein Vergleich von regulären Ausdrücken und der Bibliothek "stringsearch" in Haskell](https://fredpbytes.com/2016/05/28/regular-expressions-vs-stringsearch-in-haskell/)