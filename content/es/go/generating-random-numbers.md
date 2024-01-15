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

## ¿Por qué generar números aleatorios?

Generar números aleatorios es una práctica común en el desarrollo de software. Estos números pueden ser utilizados para probar y depurar código, crear simulaciones y juegos, entre otras aplicaciones. Además, el lenguaje de programación Go cuenta con una funcionalidad incorporada para generar números aleatorios de manera eficiente y precisa.

## Cómo hacerlo

Para generar números aleatorios en Go, utilizamos la función `rand.Intn(n)`, donde `n` representa el rango de números a generar. El siguiente código es un ejemplo de cómo generar y imprimir 5 números aleatorios entre 1 y 10:

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Establecemos la semilla del generador de números aleatorios
    // utilizando el tiempo actual
    rand.Seed(time.Now().UnixNano())

    // Generamos y mostramos 5 números aleatorios
    for i := 0; i < 5; i++ {
        fmt.Println(rand.Intn(10) + 1)
    }
}
```

La función `rand.Seed()` nos permite establecer una semilla para el generador de números aleatorios, lo que garantiza que los números generados sean realmente aleatorios. Si no establecemos una semilla, el generador utilizará una semilla por defecto, lo que puede resultar en la generación de los mismos números aleatorios en cada ejecución del programa.

## Inmersión profunda

Para generar números aleatorios más precisos y evitar posibles patrones, es importante establecer una semilla diferente en cada ejecución del programa. Una forma de hacerlo es utilizando el paquete `crypto/rand` en lugar de `math/rand`. Este paquete utiliza una fuente criptográficamente segura para generar números aleatorios, lo que garantiza una mayor aleatoriedad. 

Además, Go también ofrece la posibilidad de generar números aleatorios de diferentes tipos, como enteros de 8, 16, 32 y 64 bits, flotantes y booleanos. Puedes explorar estas funciones en la documentación oficial de Go.

## Ver también

- [Documentación oficial de Go sobre la función `rand.Intn(n)`](https://golang.org/pkg/math/rand/#Intn)
- [Documentación oficial de Go sobre el paquete `crypto/rand`](https://golang.org/pkg/crypto/rand/)