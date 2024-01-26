---
title:                "Refactoring"
date:                  2024-01-26T01:18:34.870757-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess des Umstrukturierens von bestehendem Computercode, ohne dessen externes Verhalten zu ändern. Programmierer tun dies, um nichtfunktionale Attribute der Software wie Lesbarkeit und Wartbarkeit zu verbessern, was den Code leichter verständlich machen, Komplexität reduzieren und das Auffinden von Fehlern erleichtern kann.

## Wie man es macht:
Lassen Sie uns anhand eines einfachen Beispiels für das Refactoring eines Go-Codes eintauchen. Wir nehmen einen Code-Ausschnitt, der den Durchschnitt einer Zahlenreihe berechnet und refaktorisieren ihn für mehr Klarheit und Wiederverwendbarkeit.

Ursprünglicher Code:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Durchschnitt:", average)
}
```

Refaktorisierter Code:
```Go
package main

import "fmt"

// CalculateAverage nimmt eine Slice von float64 und gibt den Durchschnitt zurück.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Durchschnitt:", average)
}
```

Im refaktorisierten Code haben wir die Logik zur Berechnung des Durchschnitts in eine separate Funktion namens `CalculateAverage` ausgelagert. Das macht die `main`-Funktion prägnanter und die Logik zur Durchschnittsberechnung wiederverwendbar und testbar.

## Tiefergehend
Code-Refactoring ist kein modernes Konzept; es existierte bereits vor der weit verbreiteten Computernutzung. Die Praxis begann wahrscheinlich im Bereich des Maschinenbaus oder sogar noch früher. In der Software wurde es mit dem Aufkommen der objektorientierten Programmierung und des Extreme Programming (XP) in den 1990er Jahren stärker formalisiert, insbesondere beeinflusst durch Martin Fowlers bahnbrechendes Buch "Refactoring: Verbesserung der Gestaltung von bestehendem Code".

Es gibt zahlreiche Refactoring-Techniken, von einfachem Umbenennen von Variablen für mehr Klarheit bis zu komplexeren Mustern wie dem Extrahieren von Methoden oder Klassen. Der Schlüssel ist, kleine, schrittweise Änderungen vorzunehmen, die die Funktionalität der Software nicht verändern, aber die interne Struktur verbessern.

Beim Einsatz von Go kann Refactoring aufgrund der Einfachheit der Sprache und der leistungsfähigen Standardbibliothek unkompliziert sein. Trotzdem ist es wichtig, einen guten Satz von Unit-Tests zu haben, um sicherzustellen, dass durch Refactoring keine Fehler eingeführt werden. Tools wie `gorename` und `gofmt` helfen, einige der Prozesse zu automatisieren, und IDEs bieten oft integrierte Unterstützung für Refactoring.

Neben manuellem Refactoring gibt es einige automatisierte Refactoring-Tools für Go, wie die Refactoring-Tools von GoLand und Go Refactor. Obwohl diese den Prozess beschleunigen können, ersetzen sie nicht das Verständnis des Codes und die Vornahme überlegter Änderungen.

## Siehe auch
 - [Refactoring in Go: Einfachheit ist schön](https://go.dev/blog/slices)
 - [Effektives Go: Refactoring mit Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowlers Refactoring-Seite](https://refactoring.com/)
 - [GoLand Refactoring-Tools](https://www.jetbrains.com/go/features/refactorings/)