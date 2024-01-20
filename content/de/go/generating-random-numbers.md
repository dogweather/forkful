---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen ist ein Prozess, bei dem eine Reihe von Zahlen generiert wird, die keiner wahrnehmbaren Musterfolge folgen. Programmierer generieren oft Zufallszahlen, um verschiedene Aufgaben wie beispielsweise Tests, Spiele oder Simulationen zu bewältigen.

## So geht's:

Hier ist ein einfaches Beispiel, wie man in Go Zufallszahlen generieren kann:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println(rand.Intn(100))
}
```

Wenn Sie dieses Programm ausführen, gibt es eine Zufallszahl zwischen 0 und 99 aus.

## Tiefere Einblicke

Historisch wurde die Generierung von Zufallszahlen durch physische Prozesse durchgeführt, wie das Rollen von Würfeln oder Ziehen von Karten. Heutzutage wird dieser Prozess meistens durch Computerprogramme realisiert.

In Go werden Zufallszahlen mit dem `rand` Paket generiert. Es gibt Alternative wie das `crypto/rand` Paket, das für kryptografische Verwendungen besser geeignet ist, da es stärkere Zufallszahlen erzeugt.

Die Generierung von Zufallszahlen in Go ist weitgehend deterministisch. Durch die Initialisierung des Zufallszahl-Generators mit `rand.Seed(time.Now().UnixNano())` ermöglichen wir es dem Programm, bei jeder Ausführung unterschiedliche Zahlen zu generieren.

## Siehe auch

Für weitere Informationen, siehe die offizielle Go Dokumentation: https://golang.org/pkg/math/rand/
 sowie weitere Beispiele und Erklärungen in der Go by Example Webseite: https://gobyexample.com/random-numbers