---
title:                "Zufallszahlen erzeugen"
html_title:           "Go: Zufallszahlen erzeugen"
simple_title:         "Zufallszahlen erzeugen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von zufälligen Zahlen ist ein wichtiger Bestandteil der Programmierung. Dabei geht es darum, Zahlen zu erzeugen, die ohne erkennbares Muster erscheinen und somit für verschiedene Anwendungen verwendet werden können. Programmierer nutzen diese Funktion, um zufällige Daten zu simulieren, Passwörter zu generieren, Spiele zu erstellen und vieles mehr.

## So geht's:

Um zufällige Zahlen in Go zu generieren, können wir die `rand`-Bibliothek nutzen. Mit der Funktion `Intn(n)` können wir eine ganze Zahl zwischen 0 und n-1 erhalten. Zum Beispiel:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	fmt.Println(rand.Intn(100)) // gibt eine Zufallszahl zwischen 0 und 99 aus
}
```

Um eine zufällige Gleitkommazahl zwischen 0 und 1 zu erhalten, können wir die Funktion `Float64()` nutzen:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	fmt.Println(rand.Float64())
}
```

## Tiefer Einblick:

Das Generieren von Zufallszahlen hat in der Informatik eine lange Geschichte. Früher wurden dafür physische Geräte wie Würfel oder Roulette-Räder verwendet. Heutzutage werden vor allem Pseudozufallszahlengeneratoren (PRNGs) verwendet, die auf bestimmte Algorithmen basieren. Es gibt auch andere Methoden wie Hardware-Zufallszahlengeneratoren, die auf physikalischen Phänomenen wie Wärmerauschen beruhen. Allerdings sind PRNGs aufgrund ihrer Effizienz und Vorhersehbarkeit die am häufigsten genutzte Methode.

Es gibt auch andere Möglichkeiten, um in Go zufällige Zahlen zu generieren, wie zum Beispiel die Nutzung von Crypto-Random-Generatoren aus der `crypto/rand`-Bibliothek. Diese verwenden kryptographische Algorithmen, um eine hohe Güte von Zufallszahlen zu gewährleisten.

Die `rand`-Funktion in Go basiert auf dem Mersenne Twister-Algorithmus, der als einer der besten PRNGs gilt. Es kann jedoch nur eine begrenzte Anzahl von Zufallszahlen generieren, bevor es sich wiederholt. Um dieses Problem zu umgehen, wird empfohlen, die Zufallszahl mit einer anderen Variable zu kombinieren, beispielsweise mit der aktuellen Systemzeit, um eine bessere Zufälligkeit zu erreichen.

## Siehe auch:

[Funktion `rand` in der Go-Dokumentation](https://golang.org/pkg/math/rand/)

[Alternative Methoden zur Generierung von Zufallszahlen in Go](https://medium.com/@deckarep/the-beast-of-responsibility-be888c255c75)