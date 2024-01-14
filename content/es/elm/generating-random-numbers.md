---
title:                "Elm: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generamos números aleatorios en Elm

Generar números aleatorios es una habilidad útil en cualquier lenguaje de programación, y Elm no es una excepción. Puede ser utilizado para crear juegos, realizar pruebas y generar datos de ejemplo. Además, agregar elementos aleatorios a un programa puede hacer que sea más interesante y divertido para el usuario.

## Cómo generar números aleatorios en Elm

Elm tiene una función incorporada llamada `Random` que permite generar números aleatorios. Aquí hay un ejemplo de cómo generar un número aleatorio entre 1 y 10 en Elm:

```Elm
import Random

Random.generate (Random.int 1 10)
```

Este código importa el módulo `Random` y utiliza la función `generate` para generar un número aleatorio mediante la función `int`, que toma un parámetro `min` para el valor mínimo y un parámetro `max` para el valor máximo. El resultado de este código sería un número aleatorio entre 1 y 10.

Pero también podemos generar números aleatorios de otros tipos de datos, como cadenas de texto o listas. Aquí hay un ejemplo de cómo generar una lista de 5 números aleatorios entre 1 y 100 en Elm:

```Elm
import Random

Random.generate (Random.list 5 (Random.int 1 100))
```

## Profundizando en la generación de números aleatorios en Elm

La función `Random.int` que utilizamos en los ejemplos anteriores en realidad es solo una abreviatura para `Random.generate` y `Random.range`. Esto significa que podemos crear nuestra propia función para generar números aleatorios. Por ejemplo, podríamos crear una función que genere un número aleatorio entre dos valores dados:

```Elm
generateRandomNumber : Int -> Int -> Int
generateRandomNumber min max =
    Random.generate (Random.int min max)
```

Con esta función, podemos generar fácilmente números aleatorios en cualquier rango que deseemos.

Además, es importante tener en cuenta que la generación de números aleatorios en Elm sigue un patrón determinista, lo que significa que si se brindan las mismas entradas, siempre se producirá el mismo resultado. Esto es útil para fines de pruebas, ya que podemos reproducir los mismos resultados cada vez.

## Ver también

- [Documentación oficial de Elm sobre la generación de números aleatorios] (https://guide.elm-lang.org/effects/random.html)
- [Una aplicación de ejemplo que utiliza la generación de números aleatorios en Elm] (https://github.com/elm-in-elm/randomizer)
- [Una discusión sobre la generación de números aleatorios en Elm en el foro de la comunidad] (https://discourse.elm-lang.org/t/a-random-discussion-about-randomness-in-elm/1588)