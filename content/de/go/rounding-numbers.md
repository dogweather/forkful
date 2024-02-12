---
title:                "Zahlen runden"
aliases:
- de/go/rounding-numbers.md
date:                  2024-02-03T18:07:43.280775-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/rounding-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Runden von Zahlen geht darum, den Wert einer Zahl auf die nächste ganze Zahl oder auf eine bestimmte Anzahl von Dezimalstellen anzupassen. Programmierer tun dies aus Gründen wie der Verbesserung der Lesbarkeit, der Vereinfachung von Berechnungen oder der Erfüllung domänenspezifischer Präzisionsanforderungen.

## Wie geht das:

In Go gibt es keine eingebaute Funktion, die Zahlen direkt auf eine bestimmte Anzahl von Dezimalstellen im Mathematikpaket rundet. Sie können jedoch das Runden durch eine Kombination von Funktionen für ganze Zahlen erreichen oder eine benutzerdefinierte Funktion für Dezimalstellen implementieren.

### Runden auf die nächste ganze Zahl:

Um auf die nächste ganze Zahl zu runden, können Sie die Funktion `math.Floor()` mit einem zusätzlichen 0,5 für positive Zahlen und `math.Ceil()` minus 0,5 für negative Zahlen verwenden, abhängig von der Richtung, in die Sie abrunden möchten.

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(math.Floor(3.75 + 0.5))  // Gibt aus: 4
	fmt.Println(math.Ceil(-3.75 - 0.5)) // Gibt aus: -4
}
```

### Runden auf eine bestimmte Anzahl von Dezimalstellen:

Für das Runden auf eine bestimmte Anzahl von Dezimalstellen kann eine benutzerdefinierte Funktion verwendet werden, bei der Sie die Zahl mit 10^n multiplizieren (wobei n die Anzahl der Dezimalstellen ist), sie wie zuvor auf die nächste ganze Zahl runden und dann durch 10^n teilen.

```go
package main

import (
	"fmt"
	"math"
)

func roundToDecimalPlace(number float64, places int) float64 {
	shift := math.Pow(10, float64(places))
	return math.Round(number*shift) / shift
}

func main() {
	fmt.Println(roundToDecimalPlace(3.14159, 2)) // Gibt aus: 3.14
	fmt.Println(roundToDecimalPlace(-3.14159, 3)) // Gibt aus: -3.142
}
```

## Tiefere Einblicke

Das Runden von Zahlen ist eine grundlegende Operation in der Computerprogrammierung, die mit der historischen Herausforderung verbunden ist, reelle Zahlen in einem Binärsystem darzustellen. Die Notwendigkeit des Rundens ergibt sich aus der Tatsache, dass viele reelle Zahlen nicht präzise in Binärform dargestellt werden können, was zu Approximationsfehlern führt.

In Go ist der Ansatz zum Runden im Vergleich zu Sprachen, die eingebaute Rundungsfunktionen für bestimmte Dezimalstellen bieten, etwas manuell. Nichtsdestotrotz bietet das `math` Paket der Go-Standardbibliothek die grundlegenden Bausteine (wie `math.Floor` und `math.Ceil`), um jede vom Anwendungsfall erforderliche Rundungsmechanik zu konstruieren.

Dieser manuelle Ansatz, der scheinbar umständlicher ist, bietet Programmierern eine feinere Kontrolle darüber, wie Zahlen gerundet werden, und dient den Präzisions- und Genauigkeitsbedürfnissen verschiedener Anwendungen. Alternativen wie Drittanbieterbibliotheken oder das Entwerfen benutzerdefinierter Rundungsfunktionen können einfachere Lösungen bieten, wenn es um komplizierte Zahlen geht oder fortgeschrittenere mathematische Operationen erforderlich sind, die nicht von der Standardbibliothek abgedeckt werden.

Zusammenfassend lässt sich sagen, dass die Go-Standardbibliothek möglicherweise keine direkte Funktionalität zum Runden auf Dezimalstellen bietet, jedoch ermöglicht ihr umfassender Satz mathematischer Funktionen Entwicklern, robuste Rundungslösungen zu implementieren, die auf ihre spezifischen Bedürfnisse zugeschnitten sind.
