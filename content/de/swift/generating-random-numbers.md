---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:50:00.884970-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen erzeugen heißt, nicht vorhersehbare Werte zu kreieren. Programmierer nutzen das für Spiele, Simulationen und alles, wo's auf Unvorhersehbarkeit ankommt.

## How to:
Swift macht's einfach. Hier ein paar Beispiele:

```Swift
// Ganze Zahlen zwischen 0 und 10
let randomInt = Int.random(in: 0...10)
print(randomInt)

// Gleitkommazahlen zwischen 0 und 1
let randomDouble = Double.random(in: 0...1)
print(randomDouble)

// Zufälliger Boolean
let randomBool = Bool.random()
print(randomBool)
```

Erwartete Ausgabe könnte sein:
```
7
0.1349238796013456
true
```

## Deep Dive
Swift verwendet für Zufallszahlen unter der Haube `arc4random_uniform()` oder neuerdings `random()` Funktionen. Seit Swift 4.2 bietet `RandomNumberGenerator` Protokoll mehr Flexibilität. Historisch wurde oft der Pseudozufallszahlengenerator (PRNG) statt echter Zufallszahlengeneratoren (RNG) eingesetzt. Swift kümmert sich darum, sodass der Default ausreicht.

Aber was, wenn's spezifisch sein muss? Es gibt `SystemRandomNumberGenerator` für's betriebssystemeigene RNG. Für Reproduzierbarkeit: eigene Generatoren. `seed()`-Methoden gestalten den Startwert handhabbar.

## See Also
- Apple's Dokumentation zum `RandomNumberGenerator` Protokoll: [Hier ansehen](https://developer.apple.com/documentation/swift/randomnumbergenerator)
- Ein Artikel über Zufallszahlen in Swift: [Hier lesen](https://www.raywenderlich.com/802-jump-start-random-number-generation-in-swift)
