---
title:    "Go: Ausgabe von Debug-Informationen"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Warum

Debug-Ausgaben sind ein wesentlicher Bestandteil einer effektiven Fehlerbehebung beim Programmieren. Sie helfen dabei, den Code zu verstehen, Probleme zu identifizieren und Lösungen zu finden.

# Wie geht man vor?

Um Debug-Ausgaben in Go zu erstellen, gibt es verschiedene Ansätze. Ein häufig verwendeter Weg ist die Verwendung der Funktion `Println` aus dem Paket `fmt`. Zum Beispiel:

```Go
package main

import "fmt"

func main() {
    num := 42
    fmt.Println("Die Variable num hat den Wert: ", num)
}
```

Dieser Code block würde die folgende Ausgabe erzeugen:

```
Die Variable num hat den Wert: 42
```

Durch Hinzufügen von `Println`-Statements in verschiedenen Teilen des Codes, können Sie den Fluss des Programms verfolgen und mögliche Fehler identifizieren.

# Tiefere Einblicke

Neben dem `Println` gibt es in Go auch die Funktion `Printf`, die eine ähnliche Funktion hat, aber zusätzlich das Formatieren von Variablen ermöglicht. Zum Beispiel:

```Go
package main

import "fmt"

func main() {
    name := "Max"
    age := 25
    fmt.Printf("Mein Name ist %s und ich bin %d Jahre alt.", name, age)
}
```

Dieser Code würde die folgende Ausgabe erzeugen:

```
Mein Name ist Max und ich bin 25 Jahre alt.
```

Eine weitere hilfreiche Funktion ist `Sprintf`, die eine formatierte Zeichenkette zurückgibt, anstatt sie auf der Konsole auszugeben. Dies ermöglicht die Verwendung von Debug-Ausgaben in komplexeren Funktionen, die eine Rückgabe haben. Zum Beispiel:

```Go
package main

import "fmt"

func main() {
    result := add(5, 7)
    fmt.Printf("5 + 7 = %d", result)
}

func add(a, b int) int {
    sum := a + b
    return sum
}
```

Dieser Code würde die folgende Ausgabe erzeugen:

```
5 + 7 = 12
```

# Siehe auch

- [Go Dokumentation - Paket fmt](https://golang.org/pkg/fmt/)
- [Code.org - Debugging in Go](https://studio.code.org/s/coursec/stage/6/puzzle/1) 
- [Golang Basics - Fehlersuche mit Println](https://www.youtube.com/watch?v=KnaBP7e0_ZA)