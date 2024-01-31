---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:45:04.376819-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Arrotondare i numeri significa modificare un numero per avvicinarlo al suo intero più vicino o ad un certo numero di decimali specificato. Si fa per semplificare i valori, renderli più leggibili, o farli rientrare in certi limiti, come quando si lavora con le valute.

## Come fare:
Il pacchetto `math` di Go è il tuo amico per l'arrotondamento. Usa `math.Round`, `math.Floor` e `math.Ceil` per semplicità:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Arrotonda al numero intero più vicino
	fmt.Println("Floor:", math.Floor(number)) // Arrotonda verso il basso
	fmt.Println("Ceil: ", math.Ceil(number))  // Arrotonda verso l'alto
}
```

Output dell'esempio:
```
Round: 3
Floor: 3
Ceil: 4
```

Per decimali specifici, moltiplica, arrotonda, quindi dividi:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Arrotondato a 2 decimali:", roundToDecimalPlace(number, 2))
}
```

Output dell'esempio:
```
Arrotondato a 2 decimali: 3.14
```

## Approfondimento
Arrotondare i numeri non è una novità: risale all'antica matematica, mirando sempre alla semplificazione. Il `math.Round` in Go utilizza l'[arrotondamento bancario](https://it.wikipedia.org/wiki/Arrotondamento#Arrotondamento_alla_pari), significando che 0,5 viene arrotondato al numero pari più vicino, riducendo un pregiudizio che potrebbe influenzare le somme.

I numeri a virgola mobile possono essere complicati a causa della loro rappresentazione binaria, che potrebbe non rappresentare esattamente tutti i decimali. Tuttavia, l'approccio di Go mantiene il comportamento previsto nella maggior parte dei casi.

Esistono altri metodi di arrotondamento, come "arrotonda per eccesso" o "arrotonda allontanandoti da zero", ma la libreria standard di Go è ciò che è prontamente disponibile. Per esigenze più complesse, potrebbe essere necessaria una libreria di terze parti o sviluppare una propria soluzione.

## Vedi anche
- Il pacchetto `math` di Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Lo standard IEEE 754 per l'aritmetica a virgola mobile (base di Go per gestire i float): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Comprendere i numeri a virgola mobile: ["Cosa ogni informatico dovrebbe sapere sull'aritmetica a virgola mobile"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
