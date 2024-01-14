---
title:    "Gleam: Generando números aleatorios"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a veces necesitamos generar números aleatorios para simular situaciones o para tomar decisiones. Con Gleam, podemos hacerlo de forma rápida y sencilla gracias a sus funciones para generar números aleatorios.

## Cómo

Para generar un número aleatorio en Gleam, podemos utilizar la función `rand.float()` que nos devuelve un número de tipo `Float` entre 0 y 1. Si queremos un número entero, podemos utilizar `rand.int()` especificando el rango deseado, por ejemplo `rand.int(1, 10)` nos dará un número entero entre 1 y 10.

```Gleam
// Generar un número aleatorio entre 0 y 1
let random_num = rand.float()

// Generar un número entero entre 1 y 10
let random_int = rand.int(1, 10)
```

Podemos también generar una lista de números aleatorios utilizando un ciclo `for` y la función `rand.int()`.

```Gleam
let numbers = for _ in [1, 2, 3, 4, 5] {
  rand.int(1, 10)
}
```

El resultado será una lista con 5 números enteros aleatorios entre 1 y 10.

## Profundizando

En Gleam, los números aleatorios son generados utilizando una fuente de entropía criptográfica segura. Esto significa que los números son verdaderamente aleatorios y no dependen de un patrón o una semilla.

Otra forma de generar números aleatorios es especificando una semilla con la función `rand.new_generator()` y luego utilizando esta semilla para generar números con `rand.int_from_generator()` o `rand.float_from_generator()`. Esto puede ser útil si queremos reproducir una serie de números aleatorios en diferentes momentos.

```Gleam
let generator = rand.new_generator(123)

// Generar un número entero a partir de la semilla
let random_num = rand.int_from_generator(generator, 1, 10)

// Generar una lista de números enteros aleatorios a partir de la semilla
let numbers = for _ in [1, 2, 3, 4, 5] {
  rand.int_from_generator(generator, 1, 10)
}
```

## Ver también

- Documentación oficial de Gleam sobre números aleatorios: https://gleam.run/std/rand.html
- Ejemplos de uso de `rand` en el repositorio oficial de Gleam: https://github.com/gleam-lang/gleam/tree/master/examples/rand