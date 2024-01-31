---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:10:45.379029-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, Ihren Code in wiederverwendbare Teile zu zerlegen. Das macht Ihren Code übersichtlicher, leichter lesbar und einfacher zu debuggen.

## Wie:
Hier ist ein Go-Snippet, das einen Codeblock zeigt, gefolgt von einer überarbeiteten Version unter Verwendung von Funktionen:

```go
package main

import "fmt"

func main() {
    // Vorher: Inline-Code
    fmt.Println("Berechne Summe...")
    total := 0
    for i := 1; i <= 10; i++ {
        total += i
    }
    fmt.Println("Gesamtsumme ist:", total)

    // Nachher: Unter Verwendung einer Funktion
    fmt.Println("Berechne Summe mit einer Funktion...")
    sum := getSum(1, 10)
    fmt.Println("Gesamtsumme ist:", sum)
}

// Funktion zur Berechnung der Summe innerhalb eines Bereichs
func getSum(start, end int) int {
    total := 0
    for i := start; i <= end; i++ {
        total += i
    }
    return total
}
```

Die Ausgabe für den Inline- und den funktionsbasierten Code wird gleich sein:

```
Berechne Summe...
Gesamtsumme ist: 55
Berechne Summe mit einer Funktion...
Gesamtsumme ist: 55
```

## Vertiefung
Bevor das Konzept der Funktionen auftauchte, war die Programmierung weitgehend prozedural, wobei der Code von oben nach unten lief. Mit wachsenden Programmen führte dieser Ansatz zu Ineffizienz und Code-Wiederholungen.

Sprachen führten Funktionen als Abstraktionsmechanismus ein. In Go kapseln Funktionen Blöcke von Code mit einer spezifischen Aufgabe ein und fördern das DRY-Prinzip (Don't Repeat Yourself - Wiederhole dich nicht). Sie akzeptieren Parameter und können Ergebnisse zurückliefern.

Nützliche Tipps:
- Benennen Sie Funktionen klar; ein guter Name erklärt, was eine Funktion tut.
- Halten Sie sie kurz; wenn eine Funktion zu viel tut, zerlegen Sie sie.
- Funktionen können mehrere Werte zurückgeben, nutzen Sie das für die Fehlerbehandlung.
- Höherwertige Funktionen (Funktionen, die andere Funktionen als Parameter nehmen oder zurückgeben) sind mächtige Werkzeuge in Go.

Alternativen zu Funktionen umfassen Inline-Code (ungeordnet für komplexe Aufgaben) und Objektmethoden (Teil des objektorientierten Paradigmas, verfügbar in Go über Structs).

## Siehe auch
- [Go by Example: Funktionen](https://gobyexample.com/functions)
- [Effective Go: Funktion](https://golang.org/doc/effective_go#functions)
