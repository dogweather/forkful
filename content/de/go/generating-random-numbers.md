---
title:                "Go: Erzeugung von zufälligen Zahlen"
simple_title:         "Erzeugung von zufälligen Zahlen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Wenn du schon immer wissen wolltest, wie du in deinen Go-Programmen Zufallszahlen generieren kannst, bist du hier genau richtig. Das Erzeugen von Zufallszahlen ist eine nützliche Fähigkeit, die in vielen Anwendungen, wie z.B. bei der Simulation von Spielen oder bei statistischen Analysen, verwendet wird.

## Wie geht man vor
Um Zufallszahlen in Go zu generieren, gibt es verschiedene Methoden, aber die einfachste Methode ist die Verwendung der `math/rand` Package. Schau dir das folgende Beispiel an:

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // Initialisiere den Zufallsgenerator
    rand.Seed(time.Now().UnixNano())
    
    // Erzeuge eine Zufallszahl zwischen 1 und 10
    randomNumber := rand.Intn(10) + 1
    
    // Gib die Zufallszahl aus
    fmt.Println("Die generierte Zufallszahl ist:", randomNumber)
}
```

Dieses kurze Programm wird eine Zufallszahl zwischen 1 und 10 erzeugen und sie auf der Konsole ausgeben. Um sicherzustellen, dass bei jedem Programmstart eine neue Zufallszahl erzeugt wird, muss der Zufallsgenerator mit `rand.Seed()` initialisiert werden. Für die Seed-Funktion wird hier die aktuelle Zeit verwendet, um sicherzustellen, dass die Zufallszahl immer unterschiedlich ist.

## Tiefergehende Informationen
Die Methode, die in diesem Beispiel verwendet wurde, um eine Zufallszahl zu generieren, ist die `Intn()` Funktion aus der `math/rand` Package. Diese Funktion gibt eine Zufallszahl vom Typ `int` zurück, die kleiner ist als der übergebene Parameter. Wenn du also eine Zufallszahl zwischen 5 und 10 erzeugen möchtest, kannst du `rand.Intn(6) + 5` verwenden.

Es ist auch möglich, Zufallszahlen vom Typ `float64` zu generieren. Dafür gibt es die Funktion `Float64()`, die eine Fließkommazahl zwischen 0 und 1 zurückgibt. Um eine Zufallszahl mit einem bestimmten Bereich zu erhalten, multipliziere einfach das Ergebnis mit der Größe des gewünschten Bereichs und addiere den Startwert.

```Go
// Erzeuge eine Zufallszahl zwischen 10 und 20
randomFloat := rand.Float64() * 10 + 10
```

Weitere Informationen über die verschiedenen Möglichkeiten, Zufallszahlen in Go zu generieren, findest du in der offiziellen Dokumentation.

## Siehe auch
- Offizielle Go-Dokumentation: https://golang.org/pkg/math/rand
- Zufallszahlen in Go - ein Tutorial: https://golangbot.com/random-numbers

Vielen Dank, dass du diesen Artikel gelesen hast. Wir hoffen, er war hilfreich und du weißt jetzt, wie du Zufallszahlen in deinen Go-Programmen verwenden kannst. Probiere es doch direkt aus und experimentiere mit verschiedenen Methoden, um deine eigenen Anwendungen zu erstellen. Viel Spaß dabei!