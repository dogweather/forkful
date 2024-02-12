---
title:                "Arbeiten mit komplexen Zahlen"
aliases: - /de/go/working-with-complex-numbers.md
date:                  2024-02-03T18:14:04.832406-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit komplexen Zahlen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit komplexen Zahlen in der Programmierung beinhaltet die Manipulation von Zahlen, die sowohl einen Real- als auch einen Imaginärteil haben, typischerweise ausgedrückt als `a + bi`. Programmierer beschäftigen sich in verschiedenen Bereichen, wie Ingenieurwesen, Physik und Datenanalyse, mit komplexen Zahlen, um Probleme zu lösen, die Quadratwurzeln von negativen Zahlen, Wellenformanalysen und mehr beinhalten.

## Wie:

In Go werden komplexe Zahlen mit den integrierten Funktionen `complex`, `real` und `imag`, zusammen mit den Typen `complex64` und `complex128` (die jeweils komplexe Zahlen mit 64 Bit und 128 Bit darstellen) behandelt. Hier ist eine Schnellstartanleitung:

```go
package main

import (
	"fmt"
)

func main() {
	// Erstellen von komplexen Zahlen
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Arithmetische Operationen
	c := a + b
	fmt.Println("Addition:", c) // Ausgabe: Addition: (3+2i)

	d := a * b
	fmt.Println("Multiplikation:", d) // Ausgabe: Multiplikation: (5+1i)

	// Zugriff auf Real- und Imaginärteile
	realTeil := real(a)
	imagTeil := imag(a)
	fmt.Printf("Realteil: %.1f, Imaginärteil: %.1f\n", realTeil, imagTeil) // Ausgabe: Realteil: 2.0, Imaginärteil: 3.0

	// Komplex konjugierte und Magnitude können berechnet werden
	konjugiert := complex(real(a), -imag(a)) // Manuell
	fmt.Println("Konjugiert von a:", konjugiert) // Ausgabe: Konjugiert von a: (2-3i)
}

```

Dieses Beispiel deckt die Grundlagen ab, aber es gibt noch viel mehr, was Sie mit komplexen Zahlen machen können, einschließlich der Nutzung des `math/cmplx` Pakets für fortgeschrittenere Operationen wie die Bestimmung der Magnitude, Phase und vieles mehr.

## Tiefere Einblicke

Das Konzept der komplexen Zahlen reicht zurück bis ins 16. Jahrhundert, erhielt jedoch erst im 19. Jahrhundert breite Anerkennung und eine strenge Formalisierung. In der Computerprogrammierung sind komplexe Zahlen seit den frühen Tagen für komplexe Arithmetik in wissenschaftlichen und ingenieurtechnischen Berechnungen unverzichtbar. Der Ansatz von Go für komplexe Zahlen, indem sie als erstklassige Bürger mit integrierter Unterstützung und umfassender Standardbibliotheksunterstützung durch das `math/cmplx` Paket behandelt werden, sticht unter den Programmiersprachen hervor. Diese Designentscheidung spiegelt Gos Schwerpunkt auf Einfachheit und Leistung wider.

Dennoch ist es erwähnenswert, dass die Arbeit mit komplexen Zahlen in Go, obwohl leistungsfähig, nicht immer der beste Ansatz für alle Anwendungen sein kann, insbesondere für solche, die symbolische Mathematik oder hochpräzise Arithmetik erfordern. Sprachen und Umgebungen, die auf wissenschaftliches Rechnen spezialisiert sind, wie Python mit Bibliotheken wie NumPy und SciPy oder Software wie MATLAB, könnten mehr Flexibilität und ein breiteres Spektrum an Funktionalitäten für spezifische Anwendungen bieten.

Dennoch, für die Systemprogrammierung und Kontexte, in denen das Integrieren von Berechnungen komplexer Zahlen in eine größere, leistungssensible Anwendung entscheidend ist, bietet Gos native Unterstützung für komplexe Zahlen eine einzigartig effiziente Option.
