---
title:                "Elm: Generando números aleatorios"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Elm?

Generar números aleatorios es una funcionalidad muy útil en programación, ya que permite crear aplicaciones con un elemento de sorpresa o aleatoriedad. En Elm, podemos generar números aleatorios de forma sencilla y confiable, lo que nos permite dar un toque especial a nuestros proyectos.

## Cómo generar números aleatorios en Elm

```Elm
import Random exposing (Generator, int)
import Random

randomNumber : Generator Int
randomNumber =
  Random.int 1 10

main =
  let
    (number, newGenerator) = Random.step randomNumber
  in
    text (toString number)
```

Este código nos permitirá generar un número aleatorio entre 1 y 10 cada vez que se ejecute el programa. También podemos especificar otro rango de números o generar números decimales utilizando la función `float` de la librería `Random`.

## Profundizando en la generación de números aleatorios en Elm

La función `Random` de Elm utiliza un algoritmo llamado Linear Congruential Generator para generar números aleatorios. Este algoritmo se basa en una fórmula matemática y una semilla inicial para producir una secuencia de números aparentemente aleatorios.

Otra cosa importante a tener en cuenta es que la generación de números aleatorios en elm es pura, lo que significa que siempre obtendremos el mismo resultado si usamos la misma semilla y la misma función. Esto evita que nuestros programas tengan comportamientos impredecibles y nos permite realizar pruebas fácilmente.

## Ver también
- [Documentación sobre la generación de números aleatorios en la página oficial de Elm](https://guide.elm-lang.org/architecture/effects/random.html)
- [Ejemplos de código para generar números aleatorios en Elm](https://elmprogramming.com/random-number-generation.html)
- [Tutorial sobre cómo integrar la generación de números aleatorios en proyectos Elm](https://medium.com/@dmi3coder/generating-random-numbers-on-the-fly-in-elm-8ced844fb515)