---
title:                "Swift: Verknüpfen von Zeichenketten"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Strings zu verketten ist eine häufig verwendete Methode in der Swift Programmierung. Durch die Zusammensetzung von mehreren Strings zu einem neuen wird es einfacher, Informationen zusammenzufügen und sie später wiederzufinden.

## Wie man es macht
Es gibt verschiedene Möglichkeiten, Strings in Swift zu verkettet. Eine Möglichkeit ist die Verwendung des "+" Operators:

```Swift
let name = "Max"
let age = 25

let greeting = "Hallo, mein Name ist " + name + " und ich bin " + String(age) + " Jahre alt."
print(greeting)
```
Output: Hallo, mein Name ist Max und ich bin 25 Jahre alt.

Eine andere Möglichkeit ist die Verwendung der `string interpolation`, bei der Werte in einen String eingefügt werden:

```Swift
let name = "Anna"
let age = 27

let greeting = "Hallo, mein Name ist \(name) und ich bin \(age) Jahre alt."
print(greeting)
```

Output: Hallo, mein Name ist Anna und ich bin 27 Jahre alt.

Man kann auch Strings mit der `append()` Funktion aneinanderreihen:

```Swift
var greeting = "Willkommen"
let name = "Sara"

greeting.append(", \(name)!")

print(greeting)
```

Output: Willkommen, Sara!

## Tiefer eintauchen
Es gibt noch einige weitere Funktionen und Möglichkeiten, Strings in Swift zu verkettet. Zum Beispiel kann man mit der `joined()` Funktion Arrays von Strings zu einem String zusammenfassen:

```Swift
let fruits = ["Apfel", "Banane", "Kirsche"]
let fruitString = fruits.joined(separator: ", ")

print("Meine Lieblingsfrüchte sind: \(fruitString).")
```

Output: Meine Lieblingsfrüchte sind: Apfel, Banane, Kirsche.

Außerdem gibt es in Swift auch die Möglichkeit, Strings mit dem `+=` Operator zu verkettet. Hier ein Beispiel:

```Swift
var greeting = "Guten Tag"
let name = "Thomas"

greeting += ", \(name)!"

print(greeting)
```

Output: Guten Tag, Thomas!

Es ist auch wichtig zu beachten, dass Strings in Swift unveränderlich sind. Das bedeutet, dass wenn man einen String verkettet oder ändert, ein neuer String erzeugt wird und der ursprüngliche String unverändert bleibt.

## Siehe auch
- [Offizielles Swift Dokumentation zu Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Häufige Fehler beim Arbeiten mit Strings in Swift](https://medium.com/@bobgodwinx/10-mistakes-swift-string-programmers-make-94e2f46f807e)
- [Tutorial zu Strings in Swift](https://medium.com/@shasheenee/swift-strings-how-to-e24ce946341c)