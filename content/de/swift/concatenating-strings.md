---
title:                "Strings verketten"
html_title:           "Swift: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Bei der Konkatenation von Strings geht es darum, zwei oder mehr Zeichenketten zusammenzufügen, um eine einzige zusammenhängende Zeichenkette zu erstellen. Programme tun dies, um effizienter mit Texten und Nachrichten zu arbeiten.

## Wie geht das?
Du kannst Strings in Swift einfach durch das Pluszeichen (+) verbinden. Du kannst auch die Methode ```.append()``` verwenden, um eine Zeichenkette an eine andere anzuhängen. Hier ist ein Beispiel:

```
let vorname = "Max"
let nachname = "Mustermann"

let vollerName = vorname + " " + nachname
// vollerName = "Max Mustermann"

vorname.append("imilian")
// vorname = "Maximilian"
```
Der Operator "+" verbindet Strings, während ```.append()``` eine Zeichenkette an eine vorhandene anhängt. Beachte, dass die Methode ```.append()``` den ursprünglichen String selbst ändert, während der "+"-Operator eine neue Zeichenkette erstellt.

## Tieferer Einblick
Die Konkatenation von Strings ist ein grundlegender und häufig verwendeter Prozess, der in vielen Programmiersprachen vorhanden ist. Es kann auch als Strings-Verkettung, Zeichenkettenverbindung oder Strings-Kombination bezeichnet werden. Alternativ kannst du auch die Methode ```.joined()``` verwenden, um mehrere Zeichenketten mithilfe eines Trennzeichens zu verbinden.

In Swift werden Strings nicht nur durch das Pluszeichen verbunden, sondern auch durch den Verkettungs-Operator "compound assignment operator" (+=). Außerdem gibt es in Swift auch Möglichkeiten, mit mehr als nur zwei Strings zu arbeiten, wie z.B. mit Arrays und der "join"-Methode.

## Weitere Quellen
Um mehr über die Konkatenation von Strings und die Verwendung in Swift zu erfahren, schau dir folgende Links an:

- [Offizielle Swift Dokumentation über Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Tutorial: Strings verketten](https://www.tutorialspoint.com/swift/swift_strings_concatenation.htm)
- [Swift Strings Crashkurs](https://zingswift.com/blog/swift-strings-cheat-sheet/)