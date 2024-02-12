---
title:                "Refaktorisierung"
aliases:
- /de/go/refactoring/
date:                  2024-02-03T18:06:55.617845-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisierung"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/refactoring.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring in der Programmierung beinhaltet die Umstrukturierung bestehenden Computer-Codes - das Ändern der Faktorisierung - ohne dessen externes Verhalten zu verändern. Programmierer*innen nehmen diesen Prozess in Angriff, um die Lesbarkeit des Codes zu verbessern, Komplexität zu verringern und die Wartbarkeit zu erhöhen, was letztendlich die Software leichter verständlich und modifizierbar macht.

## Wie:

In Go kann das Refactoring von einfachen Code-Anpassungen bis hin zu komplexeren Änderungen reichen. Beginnen wir mit einem grundlegenden Beispiel: die Vereinfachung einer anfänglichen Go-Funktion für bessere Lesbarkeit und Effizienz.

**Vor dem Refactoring:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Ausgabe: 59.9
}
```

**Nach dem Refactoring:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Ausgabe: 59.9
}
```

In der überarbeiteten Version wird `else` entfernt, was den Ablauf der Funktion vereinfacht, ohne deren Ausgabe zu beeinflussen - ein Beispiel für eine grundlegende, aber wirkungsvolle Refactoring-Technik in Go.

Für ein fortgeschritteneres Beispiel betrachten wir das Refactoring von Funktionen zur Verwendung von Schnittstellen für bessere Wiederverwendbarkeit und Testbarkeit:

**Vor dem Refactoring:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Stellen Sie sich hier einige Datenverarbeitung vor
    logger.Log("Daten verarbeitet")
}

func main() {
    logger := Logger{}
    ProcessData("Beispieldaten", logger)
}
```

**Nach dem Refactoring:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Datenverarbeitung bleibt unverändert
    logger.Log("Daten verarbeitet")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("Beispieldaten", logger)
}
```

Das Refactoring zur Nutzung einer Schnittstelle (`Logger`) anstelle eines konkreten Typs (`ConsoleLogger`) verbessert die Flexibilität der Funktion und entkoppelt die Datenverarbeitung von der spezifischen Implementierung der Protokollierung.

## Vertiefung

Refactoring in Go muss Einfachheit (eines der Kernelemente von Go) mit der in großen Softwareprojekten benötigten Flexibilität ausbalancieren. Angesichts des minimalistischen Ansatzes von Go zu Funktionen - ohne Generics (bis vor kurzem) und mit starkem Fokus auf Lesbarkeit - führt die Sprache Entwickler*innen natürlich zu einfacheren, wartbareren Code-Strukturen. Dies bedeutet jedoch nicht, dass Go-Code nicht vom Refactoring profitiert; es bedeutet, dass Refactoring immer Klarheit und Einfachheit priorisieren muss.

Historisch gesehen führte das Fehlen bestimmter Funktionen in Go (z.B. Generics vor Go 1.18) zu kreativen, aber manchmal umständlichen Lösungen für Code-Wiederverwendung und Flexibilität, was das Refactoring zur Abstraktion zu einer gängigen Praxis machte. Mit der Einführung von Generics in Go 1.18 refaktorisieren Go-Entwickler*innen nun Altcode, um dieses Feature für bessere Typsicherheit und Code-Wiederverwendung zu nutzen, was die sich entwickelnde Natur der Refactoring-Praktiken in Go zeigt.

Dennoch unterstützt Go's Toolset, einschließlich `gofmt` für Codeformatierung und `go vet` zur Identifizierung verdächtiger Konstrukte, die Beibehaltung sauberer Codebasen und reduziert den Bedarf an umfangreichem Refactoring. Während Refactoring ein unschätzbares Werkzeug im Arsenal eines Go-Programmierer*in ist, kann der weise Gebrauch von Go's Sprachfunktionen und Tools von Anfang an helfen, die Notwendigkeit für komplexe Refactorings später zu minimieren.
