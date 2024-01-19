---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generar Números Aleatorios en Elm

## ¿Qué y por qué?

La generación de números aleatorios es una manera de obtener números sin patrón predeterminado. Los programadores usan números aleatorios para características como pruebas aleatorias y simulaciones.

## Cómo hacerlo:

Generar un número aleatorio en Elm es simple. Por ejemplo:

```Elm
import Random exposing (..)

generarAleatorio : Seed -> (Int, Seed)
generarAleatorio semilla = 
    randomInt 1 100 semilla
```
Donde `semilla` es la semilla inicial para generar el número aleatorio.

Corriendo este código se obtendrá un número aleatorio entre 1 y 100 junto con una nueva semilla.

## Inmersión Profunda:

Historia: Los números aleatorios han jugado un papel importante en informática desde sus inicios.

Alternativas: Existen varias formas de generar números aleatorios, y Elm escoge una que produce resultados predecibles dada una semilla.

Detalles de Implementación: Elm maneja la aleatoriedad de manera especial, creando números aleatorios que son puramente funcionales y seguros para su uso en un entorno de programa puro.

## Consulta También: 

Enlace a la documentación oficial de Random en Elm: [Random en Elm](https://package.elm-lang.org/packages/elm/random/latest/)
Para profundizar más sobre la generación de numeros aleatorios en la computación puedes visitar: [Aleatoriedad wiki](https://es.wikipedia.org/wiki/Aleatoriedad)