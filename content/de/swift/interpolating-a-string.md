---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stringinterpolation in Swift

## Was & Warum?

Stringinterpolation ist ein Weg, um Variablen, Konstanten, Literale und Ausdrücke in einen String einzufügen, der dann vollständig ausgewertet und zu einem neuen String konvertiert wird. Sie ermöglicht es, dynamische Strings auf eine einfache und lesbare Weise zu erstellen.

## Anleitung

Sehen Sie, wie einfach es ist, die Stringinterpolation in Swift zu nutzen. 

```Swift
var name = "Florian"
print("Hallo, \(name)!")
```

Das wird folgendes angezeigt:

```Swift
Hallo, Florian!
```

Willst du eine Zahl verwenden? Kein Problem.

```Swift
var hours = 24
print("Ein Tag hat \(hours) Stunden.")
```

Die Ausgabe wird folgendes sein:

```Swift
Ein Tag hat 24 Stunden.
```

## Tiefer eintauchen

Stringinterpolation ist nicht nur in Swift, sondern auch in anderen Programmiersprachen wie Perl und Ruby seit einiger Zeit üblich und wird für ihre Einfachheit und Lesbarkeit geschätzt. Alternativen zu Stringinterpolation können die Konkatenation von Strings oder die Formatierung von Strings sein, aber beide Methoden erzeugen in der Regel mehr Code und sind weniger lesbar.

Die Implementierungsdetails der Stringinterpolation in Swift sind eigentlich ziemlich interessant. Swift verwendet eine `String Interpolation Type` Klasse, um Interpolation durchzuführen und Sie können diese Klasse erweitern und anpassen, um benutzerdefinierte Interpolationsverhalten in Ihren eigenen Typen bereitzustellen.

## Siehe auch

Für weitere Informationen über Stringinterpolation in Swift, schauen Sie bitte in die offizielle Swift-Dokumentation. Hier ein paar nützliche Links zu verwandten Themen:

- [Apple's Swift Programming Language Guide - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Custom String Interpolation in Swift](https://www.swiftbysundell.com/basics/string-interpolation/)
- [Advanced String Interpolation in Swift](https://www.swiftbysundell.com/articles/constructing-strings-in-swift/)