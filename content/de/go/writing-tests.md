---
title:    "Go: Tests schreiben"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Unit-Tests sind ein wichtiger Bestandteil der Softwareentwicklung, da sie helfen, Bugs frühzeitig zu erkennen, die Codequalität zu verbessern und das Refactoring zu erleichtern. Durch das Schreiben von Tests können Entwickler und Entwicklerinnen sicherstellen, dass ihr Code richtig funktioniert und Vertrauen in ihre Anwendungen aufbauen.

## Wie man Tests schreibt

Das Schreiben von Tests in Go ist relativ einfach und erfordert nur grundlegende Kenntnisse der Programmiersprache. Zunächst müssen wir das Paket "testing" importieren, um die notwendigen Funktionen und Methoden zum Schreiben von Tests zur Verfügung zu stellen.

```
package main

import (
    "testing"
)
```

Als nächstes erstellen wir eine Testfunktion mit dem Namen "TestAddition", die in der Regel mit dem Präfix "Test" beginnt, gefolgt von einer aussagekräftigen Beschreibung dessen, was getestet wird. Innerhalb dieser Funktion können wir Assertions verwenden, um sicherzustellen, dass der erwartete Wert zurückgegeben wird.

```
func TestAddition(t *testing.T) {
    result := add(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Berechnung war falsch, erwartetes Ergebnis: %d, erhaltenes Ergebnis: %d", expected, result)
    }
}
```

In diesem Beispiel verwenden wir die "add" Funktion, um die Addition von zwei Zahlen zu testen. Wir erwarten, dass die Funktion 5 zurückgibt, daher vergleichen wir den Rückgabewert mit der erwarteten Zahl. Wenn der Test fehlschlägt, gibt die Funktion "t.Errorf" eine Fehlermeldung aus, die uns hilft, den Fehler zu identifizieren.

Die "testify" Bibliothek bietet zusätzliche Funktionen für die Assertions, wie zum Beispiel "assert.Equal" oder "assert.NotEqual", die das Schreiben von Tests noch einfacher machen.

## Tiefere Einblicke

Das Schreiben von Tests ermöglicht es uns, verschiedene Testfälle abzudecken und sicherzustellen, dass unser Code in allen Szenarien richtig funktioniert. Wir können auch Benchmarks schreiben, um die Leistung unserer Funktionen zu messen.

Es ist auch wichtig zu beachten, dass Tests kein Ersatz für eine gründliche Codeüberprüfung und Fehlerbehebung sind. Sie sollten als Ergänzung zum Debugging-Prozess betrachtet werden und sollten regelmäßig ausgeführt werden, um sicherzustellen, dass der Code stabil bleibt.

## Siehe auch

- [Go Testing Paket Dokumentation](https://golang.org/pkg/testing/)
- [Testify Bibliothek](https://github.com/stretchr/testify)
- [Einführung in das Testen mit Go](https://medium.com/iron-io-blog/a-simple-introduction-to-testify-a526016b82d3)