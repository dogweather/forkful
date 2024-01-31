---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:40:42.479351-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen, die aus einem Real- und einem Imaginärteil bestehen (wie 5 + 7i), sind in Bereichen wie Ingenieurwesen, Physik und Signalverarbeitung unverzichtbar. Programmierer arbeiten mit ihnen, um Probleme in diesen Domänen zu lösen, die mit nur reellen Zahlen schwer zu knacken wären.

## Wie geht das:
Go unterstützt komplexe Zahlen direkt. Hier ist eine schnelle Anleitung:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Erstellen von komplexen Zahlen
	a := complex(2, 3)
	b := 4 + 5i

	// Grundoperationen
	fmt.Println("Addition:", a+b)
	fmt.Println("Subtraktion:", a-b)
	fmt.Println("Multiplikation:", a*b)
	fmt.Println("Division:", a/b)

	// Eigenschaften komplexer Zahlen
	fmt.Println("Realteil:", real(b))
	fmt.Println("Imaginärteil:", imag(b))
	fmt.Println("Konjugiert:", cmplx.Conj(b))
	fmt.Println("Betrag:", cmplx.Abs(b))
	fmt.Println("Phasenwinkel (Radianten):", cmplx.Phase(b))
}

```

Beispielausgabe:

```
Addition: (6+8i)
Subtraktion: (-2-2i)
Multiplikation: (-7+22i)
Division: (0.5609756097560976+0.0487804878048781i)
Realteil: 4
Imaginärteil: 5
Konjugiert: (4-5i)
Betrag: 6.4031242374328485
Phasenwinkel (Radianten): 0.8960553845713439
```

## Tiefergehender Einblick
Früher wurden komplexe Zahlen mit Misstrauen betrachtet – einige dachten, sie seien nutzlos! Mit der Zeit wurde ihre Macht in der Beschreibung physikalischer Phänomene klar. Sie sind grundlegend in der Quantenphysik, Regelungstechnik und Elektrotechnik, um nur einige Bereiche zu nennen.

In Go werden komplexe Zahlen mit einem Datentyp namens `complex128` (64 Bits für Real- und Imaginärteil) oder `complex64` (32 Bits jeweils) dargestellt. Unter der Haube sind dies wirklich nur zwei zusammengefügte `float64`s oder `float32`s. Die Standardbibliothek von Go, `math/cmplx`, bietet Funktionen für komplexe mathematische Operationen. Dies erspart Ihnen die mühsame Mathematik und ermöglicht es Ihnen, sich auf die Problemlösung zu konzentrieren.

Alternativen zur eingebauten Unterstützung in Go umfassen die Verwendung externer Bibliotheken oder das eigenständige Bearbeiten komplexer Zahlen. Aber diese sind selten nötig, weil die native Unterstützung von Go effizient und gut in die Sprache integriert ist.

## Siehe auch
Schauen Sie sich diese Links für mehr über Go's Fähigkeiten mit komplexen Zahlen an:
- Go's offizielle Dokumentation: https://golang.org/pkg/math/cmplx/
- Ein tieferer mathematischer Auffrischungskurs über komplexe Zahlen: https://www.mathsisfun.com/numbers/complex-numbers.html
- Praktische Anwendungen von komplexen Zahlen im Ingenieurwesen: https://ieeexplore.ieee.org/document/528dunno
