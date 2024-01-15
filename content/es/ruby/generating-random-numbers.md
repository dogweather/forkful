---
title:                "Generando números aleatorios"
html_title:           "Ruby: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas situaciones en las que necesitamos generar números aleatorios en nuestro código. Algunos ejemplos comunes incluyen juegos de azar, simulaciones científicas y pruebas de software.

## Cómo hacerlo

Podemos generar un número aleatorio en Ruby utilizando el método `rand` seguido de un paréntesis con el rango de números que queremos que se genere. Por ejemplo, si queremos un número aleatorio entre 1 y 10, podemos escribir:

```Ruby
rand(1..10) # output: 7
```

También podemos generar un número aleatorio sin un rango específico, simplemente utilizando `rand` seguido de un paréntesis vacío:

```Ruby
rand # output: 0.9983842192566451 (un número decimal aleatorio)
```

Si queremos generar un número aleatorio de punto flotante en lugar de un número entero, podemos utilizar el método `randf` de la siguiente manera:

```Ruby
randf(1.0..10.0) # output: 6.8793456943567
```

### Profundizando

Ahora que sabemos cómo generar números aleatorios en Ruby, es importante comprender cómo funciona realmente este proceso. En realidad, el método `rand` utiliza un algoritmo llamado "Generador de Números Pseudoaleatorios" (PRNG, por sus siglas en inglés). Este algoritmo toma una semilla (un número inicial) y genera una secuencia de números aparentemente aleatorios basados ​​en esa semilla.

Sin embargo, dado que la secuencia es predecible y repetitiva, no es realmente aleatoria. Por lo tanto, si queremos una verdadera aleatoriedad, necesitamos una fuente externa de ruido, como la hora actual o los movimientos del mouse, para establecer una semilla diferente cada vez que se llama al método `rand`.

## Ver también

- Documentación oficial de Ruby sobre el método `rand`: https://ruby-doc.org/core-3.0.2/Random.html#method-c-rand
- Otros métodos para generar números aleatorios en Ruby: https://medium.com/@denysdovhan/random-in-ruby-6e99bbb2b5f8