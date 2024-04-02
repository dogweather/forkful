---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:04.181688-07:00
description: "Die Fehlerbehandlung in Go umfasst das Erkennen und Reagieren auf Fehlerbedingungen\
  \ in Ihrem Programm. Programmierer betreiben Fehlerbehandlung, um\u2026"
lastmod: '2024-03-13T22:44:53.296214-06:00'
model: gpt-4-0125-preview
summary: "Die Fehlerbehandlung in Go umfasst das Erkennen und Reagieren auf Fehlerbedingungen\
  \ in Ihrem Programm. Programmierer betreiben Fehlerbehandlung, um\u2026"
title: Fehlerbehandlung
weight: 16
---

## Was & Warum?

Die Fehlerbehandlung in Go umfasst das Erkennen und Reagieren auf Fehlerbedingungen in Ihrem Programm. Programmierer betreiben Fehlerbehandlung, um sicherzustellen, dass ihre Anwendungen sich elegant von unerwarteten Situationen erholen können, was zu robusterer und zuverlässigerer Software führt.

## Wie:

In Go wird die Fehlerbehandlung explizit mit dem `error`-Typ verwaltet. Funktionen, die fehlschlagen können, geben einen Fehler als ihren letzten Rückgabewert zurück. Die Überprüfung, ob dieser Fehlerwert `nil` ist, sagt Ihnen, ob ein Fehler aufgetreten ist.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("Der Wert muss 100 oder weniger sein")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Fehler:", err)
    } else {
        fmt.Println("Ergebnis:", result)
    }
    
    // Einen Fehler elegant behandeln
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Fehler:", anotherErr)
    } else {
        fmt.Println("Ergebnis:", anotherResult)
    }
}
```

Beispielausgabe für den obigen Code:
```
Fehler: Der Wert muss 100 oder weniger sein
Ergebnis: 100
```

In diesem Beispiel gibt die Funktion `Compute` entweder einen berechneten Wert oder einen Fehler zurück. Der Aufrufer behandelt den Fehler, indem überprüft wird, ob `err` nicht `nil` ist.

## Tiefergehende Betrachtung

Gos Ansatz zur Fehlerbehandlung ist absichtlich unkompliziert und typsicher, was explizite Fehlerüberprüfungen erfordert. Dieses Konzept steht im Gegensatz zur ausnahmebasierten Fehlerbehandlung, die in Sprachen wie Java und Python zu sehen ist, wo Fehler die Aufrufkette hochpropagiert werden, es sei denn, sie werden von einem Exception-Handler abgefangen. Das Go-Team argumentiert, dass die explizite Behandlung von Fehlern zu klarerem und zuverlässigerem Code führt, da es Programmierer zwingt, Fehler sofort dort zu adressieren, wo sie auftreten.

Jedoch erwähnen einige Kritiken, dass dieses Muster zu ausführlichem Code führen kann, insbesondere in komplexen Funktionen mit vielen fehleranfälligen Operationen. Als Reaktion darauf haben neuere Versionen von Go raffiniertere Fehlerbehandlungsfunktionen eingeführt, wie das Umhüllen von Fehlern, wodurch es einfacher wird, einem Fehler Kontext zu verleihen, ohne die ursprüngliche Fehlerinformation zu verlieren. Die Community hat auch Vorschläge für neue Fehlerbehandlungsmechanismen gesehen, wie check/handle, obwohl diese zum Zeitpunkt meines letzten Updates noch in Diskussion sind.

Gos Fehlerbehandlungsphilosophie betont das Verstehen und Planen von Fehlern als Teil des normalen Programmablaufs. Dieser Ansatz fördert die Entwicklung von widerstandsfähigerer und vorhersehbarer Software, wenn auch mit einer potenziellen Zunahme an Boilerplate-Code. Alternative Muster und Bibliotheken existieren, um die Fehlerbehandlung für besonders komplexe Fälle zu vereinfachen, aber der in Go eingebaute `error`-Typ bleibt das Fundament der Fehlerbehandlung in der Sprache.
