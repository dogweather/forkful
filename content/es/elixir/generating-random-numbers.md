---
title:    "Elixir: Generación de números aleatorios"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad importante para cualquier programador en Elixir. En muchos casos, necesitamos simular datos aleatorios para pruebas o para crear juegos y aplicaciones. Afortunadamente, en Elixir hay una manera fácil de generar números aleatorios, lo que hace que nuestro código sea más eficiente y dinámico.

## Cómo hacerlo

Elixir tiene una función incorporada llamada `rand/0` que nos permite generar un número entero aleatorio. Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, podemos usar `rand(1..10)`.

```Elixir
iex> rand(1..10)
6
```

También podemos generar un número aleatorio de coma flotante entre 0 y 1 usando `Float.random/0`.

```Elixir
iex> Float.random()
0.8323526955950203
```

Si queremos generar una lista de números aleatorios, podemos usar la función `Enum.map/2` junto con `rand/0`.

```Elixir
iex> 1..10 |> Enum.map(fn(_) -> rand(1..100) end)
[34, 56, 10, 78, 43, 23, 89, 61, 5, 93]
```

También podemos especificar un número de elementos para nuestra lista aleatoria usando `Enum.map/3` y `Enum.reduce/3`.

```Elixir
iex> Enum.map(1..10, fn(_) -> rand(1..100) end, 0, &+/2)
416
```

## Profundizando

Si bien `rand/0` es fácil de usar, no siempre es eficiente en cuanto a rendimiento. Elixir también ofrece una librería llamada `:rand`, que se centra en la generación de números aleatorios más eficiente.

Además, podemos especificar una semilla para nuestro generador de números aleatorios usando `Random.seed/1`, lo que nos permite generar los mismos números aleatorios en diferentes ejecuciones del código.

## Ver también

- Documentación oficial de Elixir sobre la generación de números aleatorios: https://hexdocs.pm/elixir/Kernel.html#rand/1
- Blog post sobre la generación de números aleatorios con Elixir: https://www.badykov.com/elixir-random-number-generation/
- Ejemplos de código para generar números aleatorios en Elixir: https://github.com/xtuc/Elixir-Random-Number-Generation