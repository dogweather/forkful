---
title:    "Go: Test schreiben"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Tests ist ein wichtiger Bestandteil des Go-Programmierens. Sie helfen dabei, mögliche Fehler frühzeitig zu erkennen und die Qualität des Codes zu verbessern. Außerdem ermöglichen sie eine leichtere Wartung und Weiterentwicklung des Programms.

## Wie

Um Tests in Go zu schreiben, können Sie die integrierte "testing" Bibliothek verwenden. Diese bietet Funktionen zum Ausführen von Tests und zum Vergleichen von erwarteten und tatsächlichen Ergebnissen. Hier ist ein Beispiel für einen einfachen Test:

```Go
func TestAddition(t *testing.T) {
    result := 5 + 3
    expected := 8
    if result != expected {
        t.Errorf("Ergebnis war %v, erwartet wurde %v", result, expected)
    }
}
```
Der Codeblock "test" verwendet die Funktion "testing.T" und vergleicht das erwartete Ergebnis (8) mit dem tatsächlichen (5+3). Wenn die Bedingung nicht erfüllt ist, wird ein Fehler ausgegeben.

Sie können auch "Tabelle-Getriebene Tests" verwenden, um verschiedene Eingaben und erwartete Ausgaben zu testen. Hier ist ein Beispiel:

```Go
func TestMultiplikation(t *testing.T) {
    tests := []struct {
        a        int
        b        int
        expected int
    }{
        {2, 3, 6},
        {0, 5, 0},
        {-4, 8, -32},
    }
    for _, test := range tests {
        result := test.a * test.b
        if result != test.expected {
            t.Errorf("Falsches Ergebnis, erwartet wurde %v aber erhalten %v", test.expected, result)
        }
    }
}
```
In diesem Beispiel werden verschiedene Eingaben und erwartete Ausgaben in einer Tabelle definiert und dann in einer Schleife getestet.

## Deep Dive

Es gibt verschiedene Arten von Tests, die in Go verwendet werden können, wie zum Beispiel Unit-Tests, Integrationstests und End-to-End-Tests. Es ist wichtig, die richtige Art von Test für jedes Szenario auszuwählen. Außerdem ist es ratsam, kleinere Funktionen isoliert zu testen, um sicherzustellen, dass sie unabhängig voneinander funktionieren.

Ein weiterer wichtiger Aspekt beim Schreiben von Tests ist die Überprüfung der Fehlerbehandlung. Es ist wichtig, sicherzustellen, dass das Programm auf unerwartete Eingaben oder Ereignisse korrekt reagiert.

## Siehe auch

Hier sind einige hilfreiche Ressourcen, um mehr über das Schreiben von Tests in Go zu erfahren:

- [Offizielle Go-Dokumentation zu Tests](https://golang.org/pkg/testing/)
- [Tutorial: Testing in Go](https://blog.alexellis.io/golang-writing-unit-tests/)
- [Best Practices für das Schreiben von Tests in Go](https://medium.com/@sebdah/things-to-keep-in-mind-when-writing-go-unit-tests-7aceb59bbed8)

Vielen Dank fürs Lesen und happy coding!