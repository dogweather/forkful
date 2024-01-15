---
title:                "Generando números aleatorios"
html_title:           "Gleam: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

¿Alguna vez has necesitado generar un número aleatorio en tu programa? Tal vez para simular una situación de juego, para aplicaciones de criptografía o simplemente para tomar decisiones al azar. Sea cual sea tu razón, Gleam tiene una forma sencilla y efectiva de generar números aleatorios para satisfacer tus necesidades de programación.

## How To

No esperes más, ¡vamos a aprender cómo generar números aleatorios en Gleam!
Primero, necesitamos iniciar la librería de números aleatorios incorporada en Gleam utilizando el siguiente código:

```Gleam
import random
```

A continuación, podemos usar la función `random.int()` para generar un número entero aleatorio en un rango específico. Por ejemplo, si queremos un número aleatorio entre 1 y 10, podemos escribir el siguiente código:

```Gleam
import random

let numero = random.int(1, 10)

io.format("El número aleatorio es {}.", [numero])
```

Esto producirá un resultado como este:

```
El número aleatorio es 7.
```

También podemos generar un número aleatorio dentro de un rango específico de números decimales utilizando la función `random.float()`. Por ejemplo, si queremos un número aleatorio entre 0 y 1, podemos escribir el siguiente código:

```Gleam
import random

let numero = random.float(0.0, 1.0)

io.format("El número aleatorio es {}.", [numero])
```

Esto producirá un resultado como este:

```
El número aleatorio es 0.8725.
```

¡Pero eso no es todo! Gleam también tiene funciones para generar caracteres aleatorios (`random.char()`), booleanos aleatorios (`random.bool()`) e incluso listas aleatorias (`random.list()`). Puedes explorar más opciones de generación de números aleatorios en la documentación oficial de Gleam.

## Deep Dive

Si eres un curioso como yo, puede que te preguntes cómo es que Gleam genera estos números aleatorios. Resulta que utiliza un algoritmo de generación de números pseudoaleatorios llamado "algoritmo de Lehmer" que toma una semilla (un número inicial) para iniciar una secuencia de números que parecen ser aleatorios. Sin embargo, ten en cuenta que estos números no son realmente aleatorios, ya que se generan a partir de una fórmula matemática y pueden ser repetidos si se usa la misma semilla.

## See Also

- Documentación oficial de Gleam sobre generación de números aleatorios: https://gleam.run/lib/rand
- Más detalles sobre el algoritmo de Lehmer: https://en.wikipedia.org/wiki/Lehmer_random_number_generator