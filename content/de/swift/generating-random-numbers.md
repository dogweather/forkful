---
title:    "Swift: Generieren von Zufallszahlen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Generieren von Zufallszahlen ist ein nützliches Werkzeug, um zufällige Entscheidungen oder Variationen in Programmen einzufügen. Es kann auch verwendet werden, um Spiele oder Simulationen zu erstellen.

## Wie man Zufallszahlen generiert

Das Generieren von Zufallszahlen in Swift ist einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir die `arc4random_uniform` -Funktion importieren, die uns eine zufällige Zahl innerhalb eines bestimmten Bereichs zurückgibt. Zum Beispiel, um eine zufällige Zahl zwischen 1 und 10 zu generieren, können wir dies tun:

```Swift
let randomNum = Int(arc4random_uniform(10)) + 1
print(randomNum) // prints a random number between 1 and 10
```

Wir können auch eine Funktion erstellen, um eine bestimmte Anzahl von Zufallszahlen zu generieren und sie in einem Array zu speichern. Zum Beispiel, um ein Array mit 10 Zufallszahlen zwischen 1 und 100 zu erstellen, können wir dies tun:

```Swift
func generateRandomNumbers(quantity: Int, range: Int) -> [Int] {
    var randomNumbers = [Int]()
    for _ in 0..<quantity {
        let randomNum = Int(arc4random_uniform(UInt32(range))) + 1
        randomNumbers.append(randomNum)
    }
    return randomNumbers
}

let randomNums = generateRandomNumbers(quantity: 10, range: 100) // generates an array with 10 random numbers between 1 and 100
print(randomNums)
```
Die Ausgabe könnte zum Beispiel sein:

```Swift
[74, 2, 56, 33, 9, 45, 82, 66, 32, 13]
```

## Vertiefung

Um das Verständnis der Funktionsweise von Zufallszahlen in Swift zu vertiefen, ist es wichtig zu wissen, dass die `arc4random_uniform` -Funktion eine pseudozufällige Zahl generiert. Dies bedeutet, dass es zwar zufällig erscheinen mag, aber tatsächlich auf einem Algorithmus basiert. Die Generierung von echten Zufallszahlen in der Programmierung ist ein komplexes Thema und geht über den Rahmen dieses Artikels hinaus.

Es ist auch wichtig zu beachten, dass die in dieser Funktion verwendete Maximalzahl, in diesem Fall 10, UInt32 nicht überschreiten kann. Wenn eine größere Zahl benötigt wird, kann sie in die `Int` -Datentypkonvertiert werden.

## Siehe auch

- Dokumentation zu `arc4random_uniform` (https://developer.apple.com/documentation/swift/int/2911334-arc4random_uniform)
- Artikel zum Thema Zufallszahlen in Swift (https://learnappmaking.com/random-numbers-swift/)