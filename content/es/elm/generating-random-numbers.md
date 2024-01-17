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

## ¿Qué & ¿Por qué?
Generar números aleatorios es una técnica comúnmente utilizada en programación para producir valores no predecibles. Los programadores suelen utilizarla para incorporar elementos de azar en sus aplicaciones, como juegos o sorteos.

## Cómo hacerlo:
Para generar un número aleatorio en Elm, podemos usar la función `Random.int` que toma dos argumentos: el límite inferior y el límite superior del rango en el que se generará el número. Por ejemplo, si queremos obtener un número aleatorio entre 1 y 10, podemos escribir:

```
Elm
import Random

Random.int 1 10
```

La salida podría ser `8`, `3`, `6` o cualquier otro número entre 1 y 10.

## Profundizando:
La generación de números aleatorios ha sido un tema ampliamente estudiado en matemáticas y ciencias de la computación. A lo largo de los años, se han desarrollado muchas técnicas y algoritmos diferentes para generar secuencias de números aparentemente aleatorios. Además, existen alternativas a la función `Random.int` en Elm, como por ejemplo `Random.float`, que genera números decimales aleatorios en lugar de enteros.

## Ver también:
Puedes encontrar más información sobre la generación de números aleatorios en la [documentación oficial de Elm](https://guide.elm-lang.org/effects/random.html) y en [este artículo](https://www.mathworks.com/content/dam/mathworks/mathworks-dot-com/moler/random.pdf) escrito por Cleve Moler, uno de los pioneros en este campo.