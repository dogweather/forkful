---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was und Warum?
Zufallszahlen zu generieren bedeutet, eine Nummer in einem bestimmten Bereich zu erzeugen, die nicht vorhersehbar ist. Dies ist nützlich in vielen Bereichen der Programmierung, wie z.B. bei Spielen, zum Testen und bei Kryptographie.

## Wie man's macht:
In Swift können wir die `random(in:)` Funktion verwenden, um Zufallszahlen zu generieren. Sie muss einfach durch den gewünschten Bereich aufgerufen werden, und sie gibt uns eine Zufallszahl zurück.
```Swift
let randomInt = Int.random(in: 0..<10)
let randomDouble = Double.random(in: 0..<10)
let randomBool = Bool.random()

print(randomInt)
print(randomDouble)
print(randomBool)
```
Die Ausgabe könnte wie folgt aussehen:
```Swift
3
7.465829345460347
false
```

## Vertiefung:
Historisch gesehen wurde die Generierung von Zufallszahlen früher durch kompliziertere Algorithmen erreicht, aber seit Swift 4.2 stellt das Swift-Team uns eine einfache und intuitive Methode zur Verfügung. Alternativen könnten die Verwendung von Random Number Generators (RNGs) von Drittanbietern sein, aber in den meisten Fällen ist die eingebaute Funktion genug. Die Swift-Implementierung verwendet ein sicherer RNG, was wichtig für kritische Einsatzzwecke wie Kryptographie ist.

## Siehe auch:
- [Apple Docs zu random(in:) Funktion](https://developer.apple.com/documentation/swift/int/2995648-random) 
- [Swift.org: Random Numbers in Swift](https://swift.org/blog/se-0202/)
- [SO: How to generate random numbers in Swift](https://stackoverflow.com/questions/24007129/how-does-one-generate-a-random-number-in-apples-swift-language)