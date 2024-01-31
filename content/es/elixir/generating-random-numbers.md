---
title:                "Generación de números aleatorios"
date:                  2024-01-27T20:32:40.260311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Generar números aleatorios en Elixir es una tarea de programación fundamental, vital para aplicaciones que necesitan resultados impredecibles como en la generación de tokens seguros, el muestreo de datos o en algoritmos de juegos. Los programadores lo usan para introducir un nivel de aleatoriedad y variabilidad en sus aplicaciones, haciéndolas más dinámicas y menos deterministas.

## Cómo hacerlo:

Para generar números aleatorios en Elixir, se utiliza principalmente el módulo `:rand` que proporciona varias funciones para este propósito. Aquí tienes una guía rápida para comenzar:

Primero, asegúrate de sembrar el generador de números aleatorios para inicializarlo con un punto de partida único:

```elixir
:rand.seed(:exsplus)
```

Para generar un entero aleatorio dentro de un rango, usa:

```elixir
random_integer = :rand.uniform(10) # Genera un número entre 1 y 10
IO.puts(random_integer)
```

Para un flotante aleatorio entre 0 y 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Puede que necesites un rango más específico para flotantes, lo cual requiere un poco más de cálculo:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Recuerda, estos números son pseudoaleatorios; están determinados por la semilla y el algoritmo pero son suficientes para la mayoría de las aplicaciones.

## Estudio Profundo

Las capacidades de generación de números aleatorios de Elixir se apoyan en el módulo `:rand` de Erlang, reflejando su herencia y estrecha relación con Erlang. El módulo `:rand` reemplazó al antiguo módulo `:random`, ofreciendo algoritmos mejorados para la generación de números aleatorios. Proporciona una variedad de algoritmos, siendo `exsplus` el predeterminado, pero también soporta otros como `exs64`, `exsl`, y más, cada uno con sus compensaciones en términos de velocidad y calidad de aleatoriedad.

Un aspecto interesante de la generación de números aleatorios en Elixir (y por lo tanto en Erlang) es su manejo de las semillas. El sistema mantiene estados de semilla separados para cada proceso, asegurando que los procesos concurrentes no interfieran entre sí en sus secuencias de números aleatorios. Esto es particularmente útil en aplicaciones concurrentes, asegurando la previsibilidad y fiabilidad en sistemas distribuidos.

Aunque el módulo `:rand` es suficiente para la mayoría de los casos de uso, las aplicaciones que requieren números aleatorios criptográficamente seguros deberían considerar otras opciones. El módulo `crypto` proporciona funciones como `crypto:strong_rand_bytes/1` que están diseñadas para generar datos aleatorios seguros adecuados para propósitos criptográficos. Estas alternativas son esenciales para aplicaciones sensibles a la seguridad como la generación de tokens, la encriptación y ciertos tipos de mecanismos de autenticación.
