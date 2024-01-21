---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:24.051187-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generar números aleatorios es simplemente sacar números que no se pueden predecir fácilmente. Los programadores los usan para juegos, simulaciones, pruebas de seguridad, y donde la variedad o lo inesperado son clave.

## How to:
Genera un número aleatorio entre 0 y 10 en Go:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Inicializa la semilla del generador de números aleatorios.
	rand.Seed(time.Now().UnixNano())
	
	// Genera y muestra un número aleatorio entre 0 y 10.
	fmt.Println(rand.Intn(10)) 
}
```
Cada vez que ejecutes el programa, verás un número diferente.

## Deep Dive
La función `rand.Seed` inicializa el generador de números aleatorios con una semilla basada en el tiempo actual. Sin una semilla, `rand` generará la misma secuencia de números cada vez que se ejecute el programa. 

En el pasado, generadores como `rand` eran menos sofisticados, lo que llevó a métodos predecibles y, a veces, inseguros. Hoy en día, existen alternativas más seguras como `crypto/rand` para necesidades criptográficas.

Para garantizar que los números sean verdaderamente aleatorios, Go usa algoritmos como el generador congruencial lineal (LCG), entre otros. Aunque el paquete `math/rand` es adecuado para muchos casos, no se debe usar para criptografía debido a su previsibilidad relativa. En cambio, para esos casos usamos el paquete `crypto/rand`, que accede a fuentes más seguras de entropía del sistema operativo.

## See Also
- Documentación oficial de Go para números aleatorios: [math/rand](https://golang.org/pkg/math/rand/)
- Para seguridad criptográfica: [crypto/rand](https://golang.org/pkg/crypto/rand/)