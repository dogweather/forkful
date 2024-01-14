---
title:                "Go: Generación de números aleatorios"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una tarea común en la programación. Ya sea para simular eventos al azar o para asegurar la seguridad de una aplicación, es necesario saber cómo generar números aleatorios en Go.

## Cómo hacerlo

Go ofrece una función en su paquete `math/rand` para generar números aleatorios. Es importante recordar que estos números no son verdaderamente aleatorios y en realidad se basan en una fórmula matemática. Aún así, son suficientes para la mayoría de los casos de uso.

Para generar un número entero aleatorio entre 0 y un número máximo, utilizamos la función `Intn()` en conjunto con el paquete `rand`:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  numero := rand.Intn(100)
  fmt.Println(numero)
}
```

Este código imprimirá un número aleatorio entre 0 y 99 cada vez que se ejecute.

Si queremos generar un número aleatorio en un rango específico, podemos utilizar la función `Int()` para especificar el mínimo y el máximo:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  numero := rand.Int(50, 100)
  fmt.Println(numero)
}
```

Este código imprimirá un número aleatorio entre 50 y 99 cada vez que se ejecute.

Para generar un número decimal aleatorio, utilizamos la función `Float64()` en conjunto con la función `ExpFloat64()` para especificar el rango máximo:

```Go
package main

import (
  "fmt"
  "math/rand"
)

func main() {
  numero := rand.Float64() * rand.ExpFloat64()
  fmt.Println(numero)
}
```

La función `Float64()` generará un número decimal entre 0 y 1, y la función `ExpFloat64()` especificará un rango máximo basado en la constante matemática e (2.718...). Así que en este ejemplo, el número aleatorio generado estará entre 0 y e (2.718...).

## Profundizando

Mientras que la función `rand` en Go es suficiente para la mayoría de los casos de uso, no es verdaderamente aleatorio ya que se basa en una fórmula matemática. Para casos que requieren una verdadera aleatoriedad, podemos utilizar el paquete `crypto/rand` en conjunto con la función `Int()` para generar números aleatorios utilizando una fuente de entropía externa.

También es importante tener en cuenta que la función `rand` en Go genera siempre la misma secuencia de números cada vez que se ejecuta. Por lo tanto, si queremos una secuencia diferente cada vez, necesitaremos utilizar una semilla diferente. Esto se puede hacer con la función `Seed()`.

## Ver también

- Ejemplos de uso de la función `rand` en Go: [https://gobyexample.com/random-numbers](https://gobyexample.com/random-numbers)
- Información sobre generación de números aleatorios en Go: [https://golang.org/pkg/math/rand/](https://golang.org/pkg/math/rand/)
- Tutorial en español sobre números aleatorios en Go: [https://golang-es.org/numbers-aleatorios/](https://golang-es.org/numbers-aleatorios/)