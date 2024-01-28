---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:33:54.733886-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en Go implica el uso del paquete `math/rand` para producir números pseudoaleatorios para diversas aplicaciones como simulaciones de experimentos, generación de datos de prueba o añadiendo imprevisibilidad a juegos. Los programadores utilizan esta característica para crear comportamientos de software dinámicos y menos predecibles.

## Cómo hacerlo:

Para empezar a generar números aleatorios en Go, necesitas importar el paquete `math/rand` y el paquete `time` para sembrar el generador de números aleatorios y hacerlo más impredecible. Aquí tienes un ejemplo básico:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Sembrar el generador
	rand.Seed(time.Now().UnixNano())
	
	// Generar un entero aleatorio entre 0 y 99
	randomInt := rand.Intn(100)
	fmt.Println("Entero Aleatorio:", randomInt)
	
	// Generar un flotante aleatorio entre 0.0 y 1.0
	randomFloat := rand.Float64()
	fmt.Println("Flotante Aleatorio:", randomFloat)
}
```

El resultado del ejemplo podría ser:

```
Entero Aleatorio: 42
Flotante Aleatorio: 0.7304601899194229
```

Recuerda, cada ejecución produce números diferentes debido a la semilla con la hora actual.

## Profundización

El paquete `math/rand` en Go implementa generadores de números pseudoaleatorios (PRNGs) para varias distribuciones. Aunque bastante efectivo para muchas aplicaciones, es crucial notar que los números generados por `math/rand` no son adecuados para propósitos criptográficos debido a su naturaleza determinista. Para necesidades criptográficas, el paquete `crypto/rand` es la elección apropiada, proporcionando un generador de números aleatorios seguro.

La implementación de `math/rand` se basa en un algoritmo generador de números aleatorios subtractivo, el cual es eficiente y tiene un período relativamente largo antes de repetir secuencias. Sin embargo, para aplicaciones que requieran secuencias verdaderamente aleatorias, como operaciones criptográficas, se recomiendan generadores de números aleatorios de hardware (RNGs) o el paquete `crypto/rand`, que interactúa con fuentes de aleatoriedad seguras específicas del sistema.

`math/rand` permite sembrar para introducir variabilidad, pero la misma semilla siempre generará la misma secuencia de números, destacando la naturaleza determinista de su aleatoriedad. Esto lo hace adecuado para simulaciones o juegos donde la reproducibilidad podría ser deseable para propósitos de depuración o pruebas.
