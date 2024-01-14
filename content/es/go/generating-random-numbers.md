---
title:                "Go: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Go

Generar números aleatorios es una tarea común en la programación, especialmente en aplicaciones de juegos o simulaciones. En Go, existen varias formas de generar números aleatorios que pueden cubrir diferentes necesidades. En esta publicación, aprenderemos por qué es importante generar números aleatorios en Go y cómo hacerlo de manera eficiente.

## Cómo generar números aleatorios en Go

En Go, podemos generar números aleatorios utilizando la función `rand.Intn()` de la librería `rand`. Esta función toma un número entero como argumento y devuelve un número aleatorio entre 0 y ese número.

```Go
import (
    "fmt"
    "math/rand"
)

func main() {
    // Generar un número aleatorio entre 0 y 100
    numero := rand.Intn(101)
    fmt.Println(numero)

    // Generar cinco números aleatorios entre 50 y 100
    for i := 0; i < 5; i++ {
        numero = rand.Intn(51) + 50
        fmt.Println(numero)
    }
}

/*
Salida de ejemplo:
64
92
53
99
84
88
*/
```

Otra forma de generar números aleatorios es utilizando la función `rand.Float64()`, que devuelve un número flotante aleatorio entre 0 y 1. Podemos multiplicar este número por un valor específico para obtener un rango deseado.

```Go
// Generar un número aleatorio entre 0 y 10
numero := rand.Float64() * 10
fmt.Println(numero)
```

También podemos utilizar la función `rand.Int()` para generar un número entero aleatorio sin límites definidos.

```Go
// Generar un número entero aleatorio sin límites
numero := rand.Int()
fmt.Println(numero)
```

## Profundizando en la generación de números aleatorios

En Go, podemos establecer una semilla para generar números aleatorios utilizando la función `rand.Seed()`. Esto nos permite generar una secuencia de números aleatorios repetible, lo cual puede ser útil para propósitos de prueba o depuración.

```Go
// Establecer una semilla de 12345
rand.Seed(12345)

// Generar cinco números aleatorios entre 0 y 100
for i := 0; i < 5; i++ {
    numero = rand.Intn(101)
    fmt.Println(numero)
}

/*
Salida de ejemplo:
96
90
61
62
83
*/

// Volver a establecer la semilla de 12345
rand.Seed(12345)

// Generar cinco números aleatorios entre 0 y 100
for i := 0; i < 5; i++ {
    numero = rand.Intn(101)
    fmt.Println(numero)
}

/*
Salida de ejemplo:
96
90
61
62
83
*/

```

También podemos crear generadores de números aleatorios customizados utilizando la función `rand.New()`, que nos permite especificar una fuente de números aleatorios diferente.

```Go
// Crear un nuevo generador de números aleatorios
fuente := rand.NewSource(12345)
generador := rand.New(fuente)

// Generar cinco números aleatorios entre 0 y 100
for i := 0; i < 5; i++ {
    numero = generador.Intn(101)
    fmt.Println(numero)
}

/*
Salida de ejemplo:
96
90
61
62
83
*/
```

En resumen, generar números aleatorios en Go es una tarea sencilla gracias a las funciones proporcionadas por la librería `rand`. Sin embargo, es importante tener en cuenta la semilla y otros factores para asegurarnos de que estamos obteniendo números verdaderamente aleatorios.

## Vea también

- [Paquete rand en la documentación de Go](https://golang.org/pkg/math/rand/)
- [Tutorial sobre cómo generar números aleatorios con Go](https://www.dotnetperls.com/rand-go)
- [Video tutorial sobre la generación de números aleatorios en Go](https://www.youtube.com/watch?v=DsAhNSTY4MI)