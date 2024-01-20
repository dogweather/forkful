---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Generar números aleatorios es el proceso de producir un grupo de números que carece de cualquier patrón. Los programadores lo hacen para simular la aleatoriedad en los escenarios de prueba, para los juegos, la seguridad y las funciones de cifrado.

## Cómo hacerlo:
En Go, puedes generar números aleatorios fácilmente utilizando el paquete `math/rand`. Aquí te presento un ejemplo:
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	numAleatorio := rand.Intn(100) 
	fmt.Println(numAleatorio)
}
```
Después de ejecutar, podrías obtener una salida similar a esto:
```
47
```

## Profundizando:
1. **Contexto histórico:** A pesar de que las computadoras son máquinas deterministas y no pueden generar verdadera aleatoriedad, los algoritmos, como el usado en el paquete `math/rand` de Go, pueden producir números que son lo suficientemente aleatorios para muchos propósitos.
   
2. **Alternativas:** Como alternativa, podrías usar el paquete `crypto/rand` para generar números aleatorios criptográficamente seguros. Sin embargo, los números generados por `crypto/rand` son más lentos de producir.
 
3. **Detalles de implementación:** El generador de números aleatorios en Go es una implementación de un generador congruencial lineal, un tipo de generador de números pseudoaleatorios.

## Ver también:
- Documentación oficial de Go para [math/rand](https://golang.org/pkg/math/rand/) y [crypto/rand](https://golang.org/pkg/crypto/rand/)
- Tutorial de [Go by Example](https://gobyexample.com/random-numbers) para generar números aleatorios en Go.
- Artículo en el [Blog oficial de Go](https://blog.golang.org/go-slices-usage-and-internals) sobre cómo Go maneja los números aleatorios.