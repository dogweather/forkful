---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:48:48.862845-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Generar números aleatorios es una forma de producir valores sin un patrón predecible. Los programamos para pruebas, simulaciones, juegos y todo lo que requiera elementos de incertidumbre.

## Cómo:
En Elixir, puedes generar números aleatorios con la función `Enum.random/1`. Vamos a ver cómo usarla.

```elixir
# Para obtener un número aleatorio entre 0 y 10:
rand_num = Enum.random(0..10)
IO.puts(rand_num)
```

Sample output: `5` (Tu resultado puede variar)

Ahora, si necesitas una semilla para la generación pseudoaleatoria, puedes usar `:rand.uniform/1`.

```elixir
# Primero, inicializa la semilla
:rand.seed(:exs1024, {1234, 5678, 12345})
# Luego, genera un número aleatorio entre 1 y 10
rand_num = :rand.uniform(10)
IO.puts(rand_num)
```

Sample output: `8` (El resultado puede cambiar si modificas la semilla)

## Profundización
Los números aleatorios en Elixir y muchos otros lenguajes de programación no son realmente "aleatorios", sino pseudoaleatorios. Esto significa que derivan de algoritmos determinísticos. Para situaciones críticas donde se necesita aleatoriedad real, como criptografía, es mejor recurrir a fuentes de entropía externas.

Antes de Elixir 1.7, la generación de números aleatorios se realizaba con `:random`, que desde entonces ha quedado obsoleta y reemplazada por `:rand`, una API más robusta y eficiente.

Alternativas de generación de números aleatorios fuera del estándar de Elixir involucran dependencias externas como bibliotecas de criptografía que pueden ofrecer una mejor aleatoriedad, sobre todo para aplicaciones de seguridad.

## Ver También
- [Elixir doc for Enum.random/1](https://hexdocs.pm/elixir/Enum.html#random/1)
- [Una mirada a los algoritmos PRNG](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
