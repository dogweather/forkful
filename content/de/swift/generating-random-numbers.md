---
title:                "Generieren von Zufallszahlen"
html_title:           "Swift: Generieren von Zufallszahlen"
simple_title:         "Generieren von Zufallszahlen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Wir alle wissen, dass Zufallszahlen ein wichtiger Bestandteil der Informatik sind, aber warum sollten wir uns überhaupt damit beschäftigen? Nun, das Erzeugen von Zufallszahlen kann in vielen Anwendungsfällen sehr nützlich sein, zum Beispiel bei der Erstellung von Passwörtern, beim Testen von Software oder bei der Simulation von Ereignissen.

## Wie es geht

Um in Swift Zufallszahlen zu generieren, können wir die integrierte Funktion `arc4random_uniform()` verwenden. Diese Funktion gibt eine zufällige Zahl im angegebenen Bereich zurück. Zum Beispiel gibt `arc4random_uniform(10)` eine Zufallszahl zwischen 0 und 9 zurück. Hier ist ein Beispielcode:

```Swift
let randomNum = arc4random_uniform(10)
print(randomNum)
```
Das obige Beispiel gibt eine zufällige Zahl zwischen 0 und 9 aus. Um eine größere Zufallszahl zu generieren, können wir einfach den angegebenen Bereich ändern. Zum Beispiel gibt `arc4random_uniform(100)` eine Zufallszahl zwischen 0 und 99 zurück.

## Tiefergehende Infos

Wenn Sie genauer verstehen möchten, wie Zufallszahlen in Swift funktionieren, können Sie einen Blick auf die zugrunde liegende Methode `arc4random()` werfen. Diese Funktion generiert eine Zufallszahl basierend auf einem Entropie-Pool, der von der Kernel-Ebene verwaltet wird. Dies stellt sicher, dass die generierten Zufallszahlen tatsächlich zufällig sind und nicht vorhersehbar.

## Siehe auch

- [Swift Dokumentation zu Zufallszahlen](https://developer.apple.com/documentation/swift/int)
- [Tutorial: Zufallszahlen in Swift generieren](https://www.hackingwithswift.com/example-code/language/how-to-generate-random-numbers-in-swift)
- [Vorhersehbarkeit von Zufallszahlen vermeiden](https://stackoverflow.com/questions/10106493/)
- [Mehr zu Entropie-Pools](https://en.wikipedia.org/wiki/Entropy_(computing))