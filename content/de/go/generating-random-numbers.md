---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-27T20:33:44.882028-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in Go erfolgt durch die Verwendung des `math/rand`-Pakets, um pseudozufällige Zahlen für verschiedene Anwendungen wie die Simulation von Experimenten, die Generierung von Testdaten oder das Hinzufügen von Unvorhersehbarkeit zu Spielen zu erzeugen. Programmierer nutzen diese Funktion, um dynamische und weniger vorhersehbare Softwareverhaltensweisen zu erschaffen.

## Wie geht das:

Um mit Go Zufallszahlen zu erzeugen, müssen Sie das `math/rand`-Paket und das `time`-Paket importieren, um den Zufallszahlengenerator für mehr Unvorhersehbarkeit zu seeden. Hier ist ein einfaches Beispiel:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Den Generator seeden
	rand.Seed(time.Now().UnixNano())
	
	// Eine zufällige Ganzzahl zwischen 0 und 99 generieren
	randomInt := rand.Intn(100)
	fmt.Println("Zufällige Ganzzahl:", randomInt)
	
	// Eine zufällige Gleitkommazahl zwischen 0.0 und 1.0 generieren
	randomFloat := rand.Float64()
	fmt.Println("Zufälliger Gleitkomma:", randomFloat)
}
```

Ein mögliches Ausgabebeispiel wäre:

```
Zufällige Ganzzahl: 42
Zufälliger Gleitkomma: 0.7304601899194229
```

Denken Sie daran, dass jede Ausführung aufgrund des Seedings mit der aktuellen Zeit unterschiedliche Zahlen produziert.

## Tiefergehend

Das `math/rand`-Paket in Go implementiert pseudozufällige Zahlengeneratoren (Pseudo-Random Number Generators, PRNGs) für verschiedene Verteilungen. Obwohl es für viele Anwendungen sehr effektiv ist, ist es wichtig zu beachten, dass die von `math/rand` generierten Zahlen aufgrund ihrer deterministischen Natur nicht für kryptografische Zwecke geeignet sind. Für kryptografische Bedürfnisse ist das `crypto/rand`-Paket die richtige Wahl, da es einen sicheren Zufallszahlengenerator bereitstellt.

Die Implementierung von `math/rand` basiert auf einem subtraktiven Zufallszahlengenerator-Algorithmus, der effizient ist und eine relativ lange Periode hat, bevor Sequenzen wiederholt werden. Für Anwendungen, die wirklich zufällige Sequenzen benötigen, wie kryptografische Operationen, werden jedoch Hardware-Zufallszahlengeneratoren (Random Number Generators, RNGs) oder das `crypto/rand`-Paket, das Schnittstellen zu systemspezifischen sicheren Zufälligkeitsquellen bietet, empfohlen.

`math/rand` ermöglicht das Seeden zur Einführung von Variabilität, aber derselbe Seed wird immer dieselbe Zahlenfolge erzeugen, was die deterministische Natur seiner Zufälligkeit hervorhebt. Dies macht es geeignet für Simulationen oder Spiele, bei denen Reproduzierbarkeit für das Debugging oder Testzwecke wünschenswert sein könnte.
