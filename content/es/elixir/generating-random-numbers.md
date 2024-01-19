---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Generar números aleatorios se trata de producir números sin un patrón predecible, esenciales para numerosas aplicaciones, como juegos y criptografía.

## ¿Cómo hacerlo?
Aquí te muestro cómo generar un número aleatorio en Elixir (la versión actual), utilizando el módulo `:rand`.

```elixir
:rand.uniform() 
```
Esto genera un número aleatorio flotante entre 0.0 y 1.0. Si necesitas un número entero, puedes hacer esto:

```elixir
:rand.uniform(100) 
```
Esto te dará un número entero aleatorio entre 1 y 100. Ejemplo de la salida que genera este código:

```elixir
iex> :rand.uniform()
0.5018473922908166

iex> :rand.uniform(100)
35
```

## Análisis Profundo

1. **Contexto histórico**: El módulo `:rand` en Erlang (y por lo tanto Elixir que se ejecuta en la máquina virtual de Erlang) se implementó originalmente utilizando el algoritmo de generación de números aleatorios `random`, que ha sido depreciado en favor a `'rand'` que proporciona una mejor calidad de aleatoriedad.

2. **Alternativas**: Existen muchos otros algoritmos, cada uno con sus ventajas dependiendo de tus requerimientos. Por ejemplo, puedes utilizar el algoritmo Xorshift si necesitas generar números aleatorios muy rápido.

3. **Detalles de implementación**: En Elixir, el módulo `:rand` genera números aleatorios a través del algoritmo de mezcla multiplicativa `exrop128`. Este módulo provee una interfaz limpia para numerosas operaciones de número aleatorio, como seleccionar un número aleatorio de un rango y seleccionar un elemento aleatorio de una lista.

## Ver También
1. [Elixir :rand module documentation](https://erlang.org/doc/man/rand.html): ofrece una visión detallada del módulo `:rand`.
2. [Random number generation Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation): proporciona un panorama general y una visión más profunda sobre la generación de números aleatorios en la computación.
3. [Xorshift RNGs paper](https://www.jstatsoft.org/article/view/v008i14/xorshift.pdf): detalles del algoritmo Xorshift.