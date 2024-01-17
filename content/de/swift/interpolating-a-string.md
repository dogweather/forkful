---
title:                "Ein String interpolieren"
html_title:           "Swift: Ein String interpolieren"
simple_title:         "Ein String interpolieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Das String-Interpolieren ist eine Technik, die in der Programmierung verwendet wird, um dynamisch Werte in einen String einzusetzen. Programmierer nutzen dies, um komplexe Strings zu erstellen, die sich je nach Situation ändern können.

## How to:
Um ein String-Interpolieren in Swift zu implementieren, benötigt man ein Zeichen # vor dem Namen der Variable innerhalb des Strings. Der Wert der Variable wird dann automatisch in den String eingefügt. 

```Swift 
let name = "Mark"
let greeting = "Hallo, mein Name ist \(name)."

print(greeting) // Ausgabe: Hallo, mein Name ist Mark.
```

Ein weiteres Beispiel, um eine Variable innerhalb eines Strings zu interpolieren:

```Swift
let age = 25
let message = "Ich bin \(age) Jahre alt."

print(message) // Ausgabe: Ich bin 25 Jahre alt.
```

Man kann auch mehrere Variablen innerhalb eines Strings interpolieren:

```Swift
let name = "Lisa"
let age = 30
let occupation = "Programmiererin"

let intro = "Hallo, ich bin \(name), \(age) Jahre alt und arbeite als \(occupation)."

print(intro) // Ausgabe: Hallo, ich bin Lisa, 30 Jahre alt und arbeite als Programmiererin.
```

## Deep Dive:
Das String-Interpolieren ist eine Funktion, die in Swift standardmäßig verfügbar ist und vereinfacht die Erstellung von Strings, die dynamisch Werte enthalten sollen. Vor Swift wurde dies oft durch die Verwendung von Funktionen oder Methoden erreicht, was zeitaufwendiger und komplizierter war.

Eine Alternative zum String-Interpolieren ist die Verwendung von Stringformatierung, die ähnlich funktioniert, aber etwas komplexer ist. Hier wird ein Platzhalter im String verwendet und später durch den entsprechenden Wert ersetzt.

Das String-Interpolieren in Swift ist optimiert für Leistung und verwendet spezielle Methoden, um die Werte in den String einzufügen. Dabei wird auch die notwendige Speicherverwaltung berücksichtigt, um sicherzustellen, dass der Prozess effizient abläuft.

## See Also:
Um mehr über String-Interpolieren und seine Verwendung in Swift zu erfahren, empfehlen wir folgende Quellen:

- Offizielle Dokumentation von Apple zu Strings in Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Ein Tutorial zum String-Interpolieren in Swift von Ray Wenderlich: https://www.raywenderlich.com/172740/swift-4-string-interpolation-tutorial-getting-started