---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Go: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Sie fragen sich vielleicht, warum Sie sich über das Generieren von Zufallszahlen in Go informieren sollten. Nun, es gibt viele Situationen, in denen Sie in Ihrem Code zufällige Elemente benötigen, sei es für einfache Spiele oder komplexe Algorithmen. Das Verständnis der Techniken und Funktionen in Go zur Generierung von Zufallszahlen kann Ihnen helfen, effizientere und zuverlässigere Programme zu schreiben.

## Wie geht's

Um Zufallszahlen in Go zu generieren, müssen Sie die standardmäßige "math/rand" Bibliothek importieren. Dann verwenden Sie die Funktion `Intn(n)` , um eine zufällige Ganzzahl zwischen 0 und n-1 zu generieren. Dies kann zum Beispiel so aussehen:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	// Generiere eine zufällige Ganzzahl zwischen 0 und 9
	num := rand.Intn(10)
	fmt.Println("Die zufällige Zahl ist:", num)
}
```

Dieses Beispiel wird jedes Mal, wenn es ausgeführt wird, eine andere zufällige Zahl ausgeben.

## Tiefergehende Erklärung

Die in Go verwendete Methode zur Generierung von Zufallszahlen basiert auf einem sogenannten Pseudozufallszahlengenerator (PRNG). Das bedeutet, dass die Zahlenfolge, die vom Generator erzeugt wird, vorhersehbar ist, aber aufgrund ihrer Komplexität für den menschlichen Verstand als zufällig erscheint.

In Go wird der PRNG durch einen intern verwendeten Seed initialisiert. Wenn Sie in Ihrem Code dieselbe Seed-Zahl verwenden, erhalten Sie jedes Mal dieselbe Zahlenfolge. Um ein wirklich zufälliges Verhalten zu erzielen, können Sie die Funktion `rand.Seed()` verwenden, um den Seed basierend auf zum Beispiel aktueller Zeit oder Systemvariablen zu setzen.

Eine weitere Möglichkeit, Zufallszahlen in Go zu generieren, ist die Verwendung der "crypto/rand" Bibliothek. Diese bietet eine sicherere Methode zur Generierung von Zufallszahlen, die nicht vorhersehbar sind. Diese Methode kann jedoch etwas langsamer sein und wird in der Regel in sicherheitskritischen Anwendungen verwendet.

## Siehe auch

Hier sind einige nützliche Links, um mehr über die Generierung von Zufallszahlen in Go zu erfahren:

- [Math/rand Dokumentation](https://golang.org/pkg/math/rand/)
- [Crypto/rand Dokumentation](https://golang.org/pkg/crypto/rand/)
- [Video: Generating Random Numbers in Go](https://www.youtube.com/watch?v=UVbIMAPoo8o)
- [Post zu diesem Thema auf Go Blog](https://blog.golang.org/rand)