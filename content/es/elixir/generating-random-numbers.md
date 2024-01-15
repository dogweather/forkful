---
title:                "Generando números aleatorios"
html_title:           "Elixir: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué
Generar números aleatorios es una tarea común en muchos programas, ya sea para realizar simulaciones, generar contraseñas seguras o simplemente para agregar un elemento de sorpresa en un juego.

## Cómo hacerlo
Para generar números aleatorios en Elixir, utilizamos el módulo `:random` de la librería `:rand`, el cual contiene varias funciones útiles para trabajar con valores aleatorios.

```Elixir
# Generar un número aleatorio entre 1 y 10 (ambos incluidos)
:rand.uniform(1..10)
# => 8

# Generar un número flotante aleatorio entre 0 y 1 
:rand.uniform()
# => 0.421

# Generar una lista de 5 números aleatorios entre 1 y 100
:rand.uniform(1..100, 5)
# => [25, 68, 92, 11, 74]
```

También podemos utilizar `:rand.seed/3` para establecer una semilla y asegurarnos de obtener los mismos resultados en diferentes ejecuciones del programa.

## Profundizando en la generación de números aleatorios
Elixir utiliza el algoritmo Mersenne Twister para generar números aleatorios, el cual es considerado uno de los mejores en su categoría. Además, podemos especificar la precisión de los números flotantes utilizando la opción `:type` en `:rand.uniform/2`.

Para más información sobre la generación de números aleatorios en Elixir, puedes consultar la documentación oficial del módulo `:random` y la siguiente lectura recomendada:

## Ver también
- [Documentación de Elixir - Módulo :random](https://hexdocs.pm/elixir/Random.html)
- [The Little Elixir & OTP Guidebook - Capítulo 18: Randomness](https://www.manning.com/books/the-little-elixir-and-otp-guidebook)