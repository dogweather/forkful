---
title:                "Zahlen runden"
date:                  2024-01-26T03:44:46.837319-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zahlen runden"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zahlen zu runden bedeutet, eine Zahl auf die nächstliegende ganze Zahl oder spezifizierte Dezimalstelle anzupassen. Das wird gemacht, um Werte zu vereinfachen, sie lesbarer zu machen oder sie in bestimmte Beschränkungen einzufügen, wie etwa beim Arbeiten mit Währungen.

## Wie geht das:
Das `math`-Paket von Go ist dein Freund fürs Runden. Benutze `math.Round`, `math.Floor` und `math.Ceil` für Einfachheit:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Runden:", math.Round(number))  // Auf nächste ganze Zahl runden
	fmt.Println("Boden:", math.Floor(number)) // Abrunden
	fmt.Println("Decke: ", math.Ceil(number))  // Aufrunden
}
```

Beispielausgabe:
```
Runden: 3
Boden: 3
Decke: 4
```

Für spezifische Dezimalstellen, multipliziere, runde, dann teile:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Auf 2 Dezimalstellen gerundet:", roundToDecimalPlace(number, 2))
}
```

Beispielausgabe:
```
Auf 2 Dezimalstellen gerundet: 3.14
```

## Vertiefung
Zahlen zu runden, ist nichts Neues – es reicht zurück bis in die antike Mathematik und zielt immer auf Einfachheit ab. Das `math.Round` in Go verwendet das [Bankers' Rounding](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), was bedeutet, dass 0,5 auf die nächste gerade Zahl gerundet wird, wodurch eine Verzerrung verringert wird, die Summen beeinflussen könnte.

Gleitkommazahlen können aufgrund ihrer binären Darstellung knifflig sein, welche möglicherweise nicht genau alle Dezimalstellen darstellen kann. Der Ansatz von Go hält jedoch die meiste Zeit das erwartete Verhalten aufrecht.

Andere Rundungsmethoden existieren, wie "round half up" oder "round half away from zero", aber die Standardbibliothek von Go ist das, was unmittelbar verfügbar ist. Für komplexere Bedürfnisse könnte eine Dritt-Bibliothek benötigt werden oder man entwickelt eine eigene Lösung.

## Siehe auch
- Go's `math`-Paket: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754-Standard für Gleitkomma-Arithmetik (Grundlage von Go für den Umgang mit Floats): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Verständnis von Gleitkommazahlen: ["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)