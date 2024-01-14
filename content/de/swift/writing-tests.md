---
title:                "Swift: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-tests.md"
---

{{< edit_this_page >}}

## Warum
Das Schreiben von Tests ist ein wichtiger Bestandteil des Softwareentwicklungsprozesses, da es ermöglicht, die Funktionalität und Stabilität des Codes zu überprüfen. Es hilft dabei, Fehler frühzeitig zu erkennen und so eine höhere Codequalität zu erreichen. Zudem ist es eine gute Möglichkeit, die eigenen Fähigkeiten als Entwickler zu verbessern.

## Wie es geht
Um Tests in Swift zu schreiben, gibt es einige grundlegende Schritte zu beachten:

1. Definieren Sie die Funktion, die Sie testen möchten.
2. Legen Sie die erwarteten Ergebnisse fest.
3. Schreiben Sie den Testcode, der die Funktion aufruft und die Ergebnisse überprüft.

Ein Beispiel mit der Funktion "addition" könnte wie folgt aussehen:

```Swift
func addition(x: Int, y: Int) -> Int {
    return x + y
}
```

Um diese Funktion zu testen, können wir den folgenden Code verwenden:

```Swift
let result = addition(x: 5, y: 3)
assert(result == 8, "Erwartetes Ergebnis nicht erreicht")
```

Die Ausgabe sollte folgendermaßen aussehen:
`Assertion failed: Erwartetes Ergebnis nicht erreicht`

Dies zeigt, dass der Test erfolgreich war, da das erwartete Ergebnis übereinstimmt.

## Tiefer eintauchen
Das Schreiben von Tests kann noch effektiver werden, indem verschiedene Testarten verwendet werden, wie z.B. Unit-Tests, Integrationstests und UI-Tests. Außerdem ist es wichtig, ausreichend Codeabdeckung zu erreichen, um sicherzustellen, dass alle möglichen Szenarien getestet werden. 

Auch sollten Tests regelmäßig ausgeführt und aktualisiert werden, um sicherzustellen, dass sie immer noch korrekt sind, wenn sich der Code ändert.

## Siehe auch
- [WWDC '18 Session - "Testing Tips & Tricks"](https://developer.apple.com/wwdc18/413)
- [Apple Developer Documentation - Unit Testing with XCTest](https://developer.apple.com/documentation/xctest)
- [Swift Package Manager - Adding Tests to a Package](https://swift.org/package-manager/#adding-tests-to-a-package)