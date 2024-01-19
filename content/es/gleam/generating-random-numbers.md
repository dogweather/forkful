---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generando Números Aleatorios en Gleam: Un Vistazo Simple y Al Grano

## ¿Qué & Por qué?

Generar números aleatorios en programación significa crear números que no siguen ningún patrón predecible. Los programadores lo hacen para simular aleatoriedad en tareas como cálculos científicos, juegos, pruebas, y más.

## ¿Cómo se hace?

Ejemplos prácticos son la mejor forma de aprender. Así es cómo puedes generar números aleatorios en Gleam.

```gleam
import gleam/atom.{Atom}
import gleam/float.{from_int}
import gleam/list.{filter_map, map}
import gleam/option.{Some}
import gleam/random.{next_int}
import gleam/io.{println}
import gleam/tuple.{from_list}
import gleam/map.{from_list}

fn main() {
  let rng = next_int(in 1..10, from_int(1))
  |> expect
  |> from_int
  |> to_string
  println(rng)
}
```

Esto producirá un número aleatorio entre 1 y 10.

## Inmersión Profunda

En la programación, los números aleatorios datan desde los tiempos de las primeras computadoras. Sin embargo, Gleam, por ser un lenguaje de programación basado en Erlang, tiene una forma única de generar estos números.

La principal alternativa a `next_int` en Gleam es `next_float` que proporciona números de punto flotante aleatorios.

Asegúrate de entender que estos números no son verdaderamente aleatorios, sino pseudoaleatorios - generados por un algoritmo determinista.

## Para Investigar Más

1. Documentación oficial de Gleam: [Gleam](https://gleam.run/)
2. Un vistazo sobre generación de números aleatorios: [Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation)
3. Más detalles sobre Gleam y Erlang: [Erlang and Gleam](https://hackernoon.com/erlang-and-elixir-and-gleam-oh-my-9bb91a5300d6)