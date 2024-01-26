---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:59.999243-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen sind Daten, die nicht vorhersehbar sind. Wir erzeugen sie in Programmen für Kryptografie, Simulationen oder um einfach Unvorhersehbarkeit in Spiele zu bringen.

## How to:
Um in Go Zufallszahlen zu erzeugen, nutzen wir das `math/rand` Paket. So geht's:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) // Einen Seed basierend auf der aktuellen Zeit setzen
	zufallszahl := rand.Intn(100)    // Erzeugt eine Zufallszahl von 0 bis 99
	fmt.Println(zufallszahl)
}
```
Lauf das Programm mehrmals, und du wirst unterschiedliche Zahlen bekommen.

## Deep Dive
Eine historische Notiz: In den frühen Tagen des Computings waren Zufallszahlen schwierig zu erzeugen. Frühe Methoden nutzten physikalische Prozesse, aber heute nutzen wir Pseudozufallszahlengeneratoren (Pseudo-Random Number Generators, PRNGs), die deterministisch aber schwer vorherzusehen sind.

Alternativen in Go wären das `crypto/rand` Paket für kryptografisch sichere Zufallszahlen oder die Nutzung von Drittanbieter-Bibliotheken und -Algorithmen für spezielle Anforderungen.

Implementierungsdetail: `math/rand` verwendet einen PRNG, bekannt als "Mersenne Twister", der einen anständigen Kompromiss zwischen Schnelligkeit und Zufälligkeit bietet. Der Seed, der Initialwert, ist wichtig, ohne ihn wäre die "Zufälligkeit" vorhersehbar.

## See Also
- Die Go Dokumentation für das `math/rand` Paket: https://pkg.go.dev/math/rand
- Go Dokumentation für das `crypto/rand` Paket: https://pkg.go.dev/crypto/rand
- Ein Überblick über Zufallszahlengeneratoren: https://en.wikipedia.org/wiki/Random_number_generation
- Go Blog Beitrag über Zufallszahlengeneratoren: https://blog.golang.org/random
