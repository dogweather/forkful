---
title:                "Generando números aleatorios"
html_title:           "Go: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Generar números aleatorios es una técnica común en programación que permite crear secuencias de números aparentemente aleatorios. Los programadores utilizan esta técnica para simular situaciones aleatorias, como juegos de azar o pruebas de rendimiento.

## ¿Cómo hacerlo?

Para generar números aleatorios en Go, podemos utilizar la función `rand.Intn(n)` donde `n` es el valor máximo que queremos generar. Por ejemplo:

```
Go package main

import (
	"fmt"
	"math/rand"
)

func main() {
	fmt.Println(rand.Intn(10)) // genera un número aleatorio entre 0 y 10
	fmt.Println(rand.Intn(100)) // genera un número aleatorio entre 0 y 100
}
```

Este código imprimirá dos números aleatorios cada vez que se ejecute. 

## Profundizando

Antes de la aparición de las computadoras, los científicos y matemáticos utilizaban métodos físicos para generar números aleatorios, como lanzar dados o seleccionar tarjetas de forma aleatoria. Sin embargo, con el avance de la tecnología, se han desarrollado algoritmos y programas más eficientes para generar números aleatorios. En Go, también puedes utilizar la función `rand.Float64()` para generar números aleatorios con decimales.

Hay otras formas de generar números aleatorios en Go, como utilizando la librería `crypto/rand` para generar números criptográficamente seguros. Sin embargo, la función `rand.Intn(n)` es suficiente para la mayoría de los casos.

## Ver también

- Documentación oficial de la función `rand`: https://golang.org/pkg/math/rand/
- Ejemplo de uso de `crypto/rand`: https://gobyexample.com/random-numbers