---
title:                "Redondeo de números"
date:                  2024-01-26T03:44:37.092494-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Redondear números significa ajustar un número a su entero más cercano o a un lugar decimal especificado. Se hace para simplificar valores, hacerlos más legibles, o ajustarlos a ciertas restricciones, como cuando se trabaja con monedas.

## Cómo hacerlo:
El paquete `math` de Go es tu amigo para redondear. Usa `math.Round`, `math.Floor` y `math.Ceil` por simplicidad:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Redondear al entero más cercano
	fmt.Println("Floor:", math.Floor(number)) // Redondear hacia abajo
	fmt.Println("Ceil: ", math.Ceil(number))  // Redondear hacia arriba
}
```

Salida de muestra:
```
Round: 3
Floor: 3
Ceil: 4
```

Para lugares decimales específicos, multiplica, redondea, luego divide:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Redondeado a 2 lugares decimales:", roundToDecimalPlace(number, 2))
}
```

Salida de muestra:
```
Redondeado a 2 lugares decimales: 3.14
```

## Estudio Profundo
El redondeo de números no es nuevo, se remonta a la matemática antigua, buscando siempre la simplicidad. `math.Round` en Go usa el [redondeo bancario](https://es.wikipedia.org/wiki/Redondeo#Redondeo_hacia_el_número_par_más_cercano), lo que significa que 0.5 se redondea al número par más cercano, reduciendo un sesgo que podría afectar las sumas.

Los números de punto flotante pueden ser complicados debido a su representación binaria, que puede no representar exactamente todos los decimales. Sin embargo, el enfoque de Go mantiene el comportamiento esperado la mayoría del tiempo.

Existen otros métodos de redondeo, como "redondear hacia arriba" o "redondear alejándose de cero", pero la biblioteca estándar de Go es lo que está disponible de manera inmediata. Para necesidades más complejas, es posible que necesites una biblioteca de terceros o desarrollar tu propia solución.

## Ver También
- Paquete `math` de Go: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- Estándar IEEE 754 para aritmética de punto flotante (base de Go para manejar flotantes): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Entendiendo el punto flotante: ["Lo que todo informático debería saber sobre aritmética de punto flotante"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
