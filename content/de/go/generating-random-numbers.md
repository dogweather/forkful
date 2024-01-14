---
title:    "Go: Zufällige Zahlen generieren"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal eine Anwendung geschrieben hast, die auf Zufallszahlen angewiesen war, weißt du wahrscheinlich, wie wichtig es ist, effektive und zuverlässige Methoden zur Generierung dieser Zahlen zu haben. In diesem Blog-Beitrag werden wir uns mit der Generierung von Zufallszahlen in der Programmiersprache Go beschäftigen und wie du diese Funktionen in deinen Projekten nutzen kannst.

## Wie geht man vor

Um Zufallszahlen in Go zu generieren, gibt es zwei Hauptmethoden: die Verwendung der "math/rand" und "crypto/rand" Packages. Für die meisten Anwendungen ist "math/rand" ausreichend, da es schneller und einfacher zu implementieren ist. Hier ist ein Beispielcode, wie du Zufallszahlen mit "math/rand" generieren kannst:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  rand.Seed(time.Now().UnixNano()) // sorgt für eine einzigartige Zufallszahl pro Ausführung

  // Generiere eine Zufallszahl zwischen 1 und 100
  random := rand.Intn(100) + 1

  fmt.Println("Deine zufällige Zahl ist:", random)
}
```

Dieses Beispiel verwendet die Funktion "Intn", um eine Zufallszahl innerhalb einer bestimmten Range zu generieren. "Seed" wird verwendet, um sicherzustellen, dass bei jeder Ausführung des Codes eine neue Zufallszahl generiert wird.

## Tiefergehende Informationen

Wenn du eine höhere Qualität und Sicherheit bei den generierten Zufallszahlen benötigst, solltest du das "crypto/rand" Package verwenden. Dieses Package nutzt eine kryptografisch sichere Zufallszahlengenerierung und ist vorhersehbarer als die in "math/rand" verwendeten Methoden. Hier ist ein Beispielcode, wie du Zufallszahlen mit "crypto/rand" generieren kannst:

```Go
package main

import (
  "crypto/rand"
  "fmt"
  "math/big" // wichtig für die Verwendung von kryptografisch sicheren Zufallszahlen 
)

func main() {
  random, err := rand.Int(rand.Reader, big.NewInt(100))
  if err != nil {
    fmt.Println("Fehler beim Generieren der Zufallszahl:", err)
    return
  }

  fmt.Println("Deine zufällige Zahl ist:", random)
}
```

Hier sehen wir, dass wir eine Instanz von "rand.Reader" verwenden müssen, und "big.NewInt" gibt die gewünschte Range an. Wenn ein Fehler auftritt, werden wir eine entsprechende Nachricht ausgegeben.

## Siehe auch

- <https://golang.org/pkg/math/rand/>
- <https://golang.org/pkg/crypto/rand/>
- <https://blog.golang.org/rand>
- <https://tour.golang.org/basics/49>