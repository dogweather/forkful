---
title:    "Elm: Generando números aleatorios"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Elm?

Generar números aleatorios es una práctica común en la programación que puede ser útil en una variedad de situaciones. Con la ayuda de la función `Random.elm`, puedes generar números aleatorios en Elm de manera simple y efectiva para usarlos en tus aplicaciones.

## Cómo hacerlo

Para generar números aleatorios en Elm, primero necesitas importar el módulo `Random` en tu archivo. Luego, puedes usar la función `Random.generate` y pasarle una semilla y una función que detalla cómo quieres que se genere el número aleatorio. Por ejemplo:

```Elm
import Random

-- Genera un número aleatorio entre 1 y 10
Random.generate (Random.int 1 10) Random.initialSeed
--> 7 : Random.Seed -> Int

-- Genera una lista de 5 números aleatorios entre 1 y 100
Random.generate (Random.list 5 (Random.int 1 100)) Random.initialSeed
--> [55, 27, 69, 48, 11] : Random.Seed -> List Int
```

## Profundizando en la generación de números aleatorios

En Elm, la generación de números aleatorios se basa en el concepto de semilla (seed). Una semilla es un valor que se usa para inicializar el generador de números aleatorios. Al utilizar `Random.initialSeed`, obtienes una semilla determinada por el reloj del sistema. Sin embargo, también puedes definir tu propia semilla para obtener una secuencia específica de números aleatorios.

Además, la función `Random.int` toma dos parámetros que indican el rango dentro del cual quieres generar el número aleatorio. También puedes usar otras funciones del módulo `Random`, como `float`, `bool` o `pair`, para generar diferentes tipos de valores aleatorios.

## Ver también

- Documentación de Elm sobre `Random`: https://package.elm-lang.org/packages/elm/random/latest/
- Ejemplo de generación de números aleatorios en una aplicación de juegos en Elm: https://steveperry-53.github.io/elm-game/
- Preguntas frecuentes sobre generación de números aleatorios en Elm: https://discourse.elm-lang.org/t/random-state-and-code-origination/2961