---
title:                "Generando números aleatorios"
html_title:           "Elm: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una habilidad útil en la programación ya que te permite crear programas más dinámicos e interactivos.

## Cómo hacerlo

Para generar números aleatorios en Elm, puedes utilizar la función `Random.generate`. Esta función toma dos argumentos: un generador de números aleatorios y una función para manejar los resultados.

Un ejemplo sencillo sería generar un número aleatorio entero entre 1 y 10:

```elm
import Random

Random.generate (\_ -> 1..10) Maybe.Nothing
```

Si ejecutamos este código, obtendremos un número aleatorio del 1 al 10 cada vez que lo hagamos. Pero, ¿cómo funciona esto?

En este caso, estamos utilizando la función `1..10` como generador de números aleatorios. Esta función simplemente devuelve un número aleatorio dentro de ese rango cuando se le llama. Luego, utilizamos `Maybe.Nothing` como función de manejo de resultados, lo cual nos permite ignorar el resultado y simplemente imprimir el número.

En casos más complejos, podríamos querer almacenar el resultado en una variable o utilizarlo en un cálculo. En ese caso, podemos utilizar la función `Maybe.Just` para manejar el resultado de manera más específica.

Elm también ofrece otras funciones para generar números aleatorios, como `Random.float` para generar números aleatorios con decimales y `Random.int` para generar números aleatorios con rangos específicos.

## Profundizando

Si quieres profundizar en la generación de números aleatorios en Elm, puedes utilizar el módulo `Random`. Este módulo te permite crear generadores personalizados y combinarlos para generar resultados más complejos.

Por ejemplo, podríamos crear un generador para obtener una combinación de colores RGB aleatoria:

```elm
import Random

randomColor : Random.Generator (Int, Int, Int)
randomColor =
    Random.map3 (,,)
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)

Random.generate randomColor Maybe.Nothing
```

Este código creará un generador que devuelve una tupla de tres números aleatorios entre 0 y 255. También podemos utilizar `Random.spawn` para obtener una tupla de valores en lugar de un resultado de `Maybe`:

```elm
Random.spawn randomColor
```

Con un poco de creatividad, puedes utilizar estos generadores personalizados para crear todo tipo de resultados aleatorios en tus programas.

## Ver también
- [Documentación oficial de Random en Elm](https://package.elm-lang.org/packages/elm/random/latest/)
- [Ejemplos de generación de números aleatorios en Elm](https://github.com/elm-lang/example-apps/tree/master/random)