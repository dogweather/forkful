---
title:                "Haskell: Verwendung von regulären Ausdrücken"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke können eine nützliche Technik beim Programmieren sein, die es ermöglicht, komplexe Textmuster zu erkennen und zu verarbeiten. Sie sind ein wichtiger Bestandteil der funktionalen Programmierung und können dabei helfen, effiziente und elegante Lösungen zu entwickeln.

## Wie 

Um reguläre Ausdrücke in Haskell zu verwenden, müssen Sie zunächst das Modul "Text.Regex.Posix" importieren. Dann können Sie die Funktion "matchRegex" nutzen, um eine reguläre Ausdrucksabfrage auf einen String anzuwenden. Zum Beispiel:

```
import Text.Regex.Posix

inputString = "Hallo, meine Telefonnummer ist 0123456789."

-- Suche nach Telefonnummer mit 10 Ziffern
phoneRegex = "([0-9]{10})"

-- Verwende matchRegex, um die Telefonnummer zu finden
result = matchRegex phoneRegex inputString
```

Die Variable "result" enthält nun eine Liste, die alle Übereinstimmungen mit dem regulären Ausdruck enthält, in diesem Fall eine Liste mit der Telefonnummer "0123456789". Sie können auch benannte Gruppen in Ihrem regulären Ausdruck definieren, um bestimmte Teile des Musters zu extrahieren.

## Tieferer Einblick

Reguläre Ausdrücke folgen bestimmten Regeln, um Textmuster zu beschreiben. Diese Regeln können manchmal kompliziert werden, vor allem wenn man mit speziellen Zeichen wie "*" oder "+" umgehen muss. Es gibt jedoch viele Online-Ressourcen, die dabei helfen können, diese Regeln zu verstehen und zu meistern.

Ein weiterer wichtiger Aspekt von regulären Ausdrücken in Haskell ist die Performance. Verwenden Sie "matchRegex" nur dann, wenn Sie wirklich einen vollständigen String-Match benötigen. Wenn Sie nur Teile des String-Musters benötigen, kann es effizienter sein, "subRegex" oder "applyRegex" zu verwenden.

## Siehe auch

- [Haskell Dokumentation zu regulären Ausdrücken](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html)
- [Eine ausführliche Einführung in reguläre Ausdrücke in Haskell](https://wiki.haskell.org/Parsing_a_simple_imperative_language)
- [Online reguläre Ausdruck Tester für Haskell](https://regex-tester.com/haskell)