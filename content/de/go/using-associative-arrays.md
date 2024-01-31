---
title:                "Verwendung von assoziativen Arrays"
date:                  2024-01-30T19:11:11.888569-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, die in Go als Maps bekannt sind, ermöglichen es Ihnen, Daten in Schlüssel-Wert-Paaren zu speichern und darauf zuzugreifen. Sie sind unerlässlich für die Verwaltung von Sammlungen, bei denen Sie Werte schnell über einen einzigartigen Schlüssel suchen können, was die Datenmanipulation und -abfrage in Ihren Programmen vereinfacht.

## Wie geht das:

In Go sind Maps unkompliziert zu verwenden. Hier ist eine einfache Anleitung, um zu beginnen:

1. **Deklarieren und Initialisieren von Maps**

```Go
package main

import "fmt"

func main() {
    // Initialisiert eine leere Map mit String-Schlüsseln und Int-Werten
    var scores map[string]int
    fmt.Println(scores) // Gibt aus: map[]

    // Deklarieren und Initialisieren einer nicht-leeren Map
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Gibt aus: map[green:#00ff00 red:#ff0000]
}
```

2. **Elemente hinzufügen und darauf zugreifen**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Gibt aus: 5
}
```

3. **Über Maps iterieren**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s macht %s\n", key, value)
    }
    // Ausgabereihenfolge kann variieren, da Maps keine Reihenfolge garantieren.
}
```

4. **Elemente löschen**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Vor dem Löschen

    delete(meals, "lunch")
    fmt.Println(meals) // Nach dem Löschen
}
```

## Vertiefung

Eingeführt in Go 1, bieten Maps eine integrierte Möglichkeit, assoziative Arrays effizient zu handhaben. Im Gegensatz zu Slices, die geordnete Sammlungen sind, sind Maps ungeordnet. Das bedeutet, dass die Iterationsreihenfolge über Map-Elemente nicht garantiert ist, dass sie bei verschiedenen Ausführungen gleich bleibt, ein Kompromiss für ihre Fähigkeit, Schlüssel-Wert-Paare dynamisch und mit erheblicher Flexibilität zu handhaben.

Unter der Haube implementiert Go Maps als Hashtabellen und gewährleistet, dass die durchschnittliche Komplexität von Zugriffs-, Einfüge- und Löschoperationen unter den meisten Umständen O(1) ist. Es ist jedoch zu beachten, dass diese Effizienz aufgrund von Faktoren wie Hash-Kollisionen variieren kann.

Für Anwendungsfälle, die eine geordnete Schlüsseldurchquerung erfordern, könnten Sie in Erwägung ziehen, Maps mit Slices zu kombinieren oder Drittanbieter-Pakete zu erkunden, die zusätzliche Datenstrukturen wie geordnete Maps oder Bäume anbieten. Trotz ihrer Einschränkungen sind Maps von Go ein mächtiges und unerlässliches Werkzeug für viele Programmierszenarien.
