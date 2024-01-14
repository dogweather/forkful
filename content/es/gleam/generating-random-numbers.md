---
title:                "Gleam: Generando números aleatorios"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una de las funcionalidades más útiles en la programación. Puede ser útil para juegos, generación de datos de prueba, y muchas otras aplicaciones. En esta publicación, veremos cómo generar números aleatorios en Gleam y cómo utilizarlos en tu propio código.

## Cómo hacerlo

Para generar números aleatorios en Gleam, podemos utilizar la función `random.uniform` del módulo `gleam/random`.

```Gleam
import gleam/random

// Genera un número aleatorio entre 0 y 1
let number = random.uniform(0, 1)
```

También podemos especificar un rango de números en el que queremos que se genere el número aleatorio, por ejemplo, de 1 a 10:

```Gleam
import gleam/random

// Genera un número aleatorio entre 1 y 10
let number = random.uniform(1, 10)
```

Otra opción es generar números enteros aleatorios utilizando la función `random.uniform_integer`:

```Gleam
import gleam/random

// Genera un número entero aleatorio entre 0 y 10
let number = random.uniform_integer(0, 10)
```

Para un uso más avanzado, podemos utilizar la función `random.generate` para generar una cantidad determinada de números aleatorios en una lista:

```Gleam
import gleam/random

// Genera 5 números aleatorios entre 0 y 10 y los almacena en una lista
let numbers = random.generate(5, 0, 10)
```

## Profundizando

La generación de números aleatorios en Gleam utiliza el algoritmo Mersenne Twister, que es una de las técnicas más utilizadas en la generación de números aleatorios en los lenguajes de programación.

Además, es importante tener en cuenta que la generación de números aleatorios no es realmente "aleatoria", ya que se basa en un algoritmo y una semilla (un valor inicial) que determina la secuencia de números generados. Por lo tanto, si se proporciona la misma semilla, se generará la misma secuencia de números aleatorios.

## Ver también

- Documentación oficial de Gleam sobre la generación de números aleatorios: https://gleam.run/modules/random.html
- Artículo sobre el algoritmo Mersenne Twister: https://es.wikipedia.org/wiki/Mersenne_Twister