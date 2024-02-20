---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:39.883120-07:00
description: "Assoziative Arrays, in Go als Maps bekannt, erm\xF6glichen es dir, Schl\xFC\
  ssel-Wert-Paare zu speichern, bei denen jeder einzigartige Schl\xFCssel auf einen\
  \ Wert\u2026"
lastmod: 2024-02-19 22:05:12.324810
model: gpt-4-0125-preview
summary: "Assoziative Arrays, in Go als Maps bekannt, erm\xF6glichen es dir, Schl\xFC\
  ssel-Wert-Paare zu speichern, bei denen jeder einzigartige Schl\xFCssel auf einen\
  \ Wert\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, in Go als Maps bekannt, ermöglichen es dir, Schlüssel-Wert-Paare zu speichern, bei denen jeder einzigartige Schlüssel auf einen Wert verweist. Programmierer verwenden Maps für effizientes Datenabrufen, Modifizieren und zur Pflege einer Sammlung von Elementen, auf die schnell mit einzigartigen Schlüsseln zugegriffen werden kann.

## Wie man:

Das Erstellen und Initialisieren einer Map in Go kann auf verschiedene Arten erfolgen. Hier ist ein einfaches Beispiel, um zu beginnen:

```go
package main

import "fmt"

func main() {
    // Deklarieren und Initialisieren einer Map
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Ausgabe: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

Um Elemente hinzuzufügen oder zu aktualisieren, weist du einem Schlüssel einen Wert zu, so:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Ausgabe: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Auf einen Wert über seinen Schlüssel zuzugreifen ist unkompliziert:

```go
fmt.Println("Der Hex-Code für Rot ist:", colors["red"])
// Ausgabe: Der Hex-Code für Rot ist: #FF0000
```

Um ein Element zu löschen, verwende die `delete` Funktion:

```go
delete(colors, "red")
fmt.Println(colors)
// Ausgabe: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Das Iterieren über eine Map erfolgt mit einer For-Schleife:

```go
for color, hex := range colors {
    fmt.Printf("Schlüssel: %s Wert: %s\n", color, hex)
}
```

Denke daran, dass Maps in Go ungeordnet sind. Die Reihenfolge der Iteration ist nicht garantiert.

## Tiefer eintauchen

In Go sind Maps als Hashtabellen implementiert. Jeder Eintrag in der Map besteht aus zwei Elementen: einem Schlüssel und einem Wert. Der Schlüssel wird gehasht, um den Eintrag zu speichern, was Operationen in konstanter Zeit für eine kleine Menge an Daten und eine durchschnittliche Zeitkomplexität von O(1) mit ordentlichem Hashing ermöglicht, die sich im schlimmsten Fall mit vielen Hash-Kollisionen auf O(n) verschlechtern kann.

Ein wichtiger Hinweis für neue Go-Programmierer ist, dass Map-Typen Referenztypen sind. Das bedeutet, wenn du eine Map an eine Funktion übergibst, sind alle Änderungen, die innerhalb dieser Funktion an der Map vorgenommen werden, für den Aufrufer sichtbar. Das unterscheidet sich beispielsweise von der Übergabe eines Structs an eine Funktion, bei der der Struct kopiert wird, es sei denn, er wird per Pointer übergeben.

Während Maps für die meisten Anwendungsfälle mit assoziativen Arrays unglaublich vielseitig und effizient sind, kann es in leistungskritischen Anwendungen vorteilhaft sein, Datenstrukturen mit vorhersehbareren Leistungscharakteristiken zu verwenden, insbesondere wenn Schlüsselverteilungen häufige Kollisionen verursachen können.

Eine weitere Alternative ist der `sync.Map`, der seit Go 1.9 verfügbar ist, konzipiert für Anwendungsfälle, in denen Schlüssel nur einmal geschrieben, aber viele Male gelesen werden, was in diesen Szenarien Effizienzverbesserungen bietet. Für herkömmliche Go-Anwendungen ist jedoch die reguläre Map-Nutzung idiomatisch und oft der empfohlene Ansatz wegen ihrer Einfachheit und dem direkten Support in der Sprache.
