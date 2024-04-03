---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:26.385044-07:00
description: "Escribir pruebas en Elm implica crear casos de prueba para verificar\
  \ la correcci\xF3n de tu c\xF3digo Elm, asegurando que se comporte como se espera.\
  \ Los\u2026"
lastmod: '2024-03-13T22:44:58.996358-06:00'
model: gpt-4-0125-preview
summary: "Escribir pruebas en Elm implica crear casos de prueba para verificar la\
  \ correcci\xF3n de tu c\xF3digo Elm, asegurando que se comporte como se espera."
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:
Elm utiliza el paquete `elm-explorations/test` para escribir pruebas unitarias y pruebas de fuzz. Comienza añadiendo el paquete a tu proyecto:

```elm
elm install elm-explorations/test
```

Crea un archivo de prueba, digamos `tests/ExampleTest.elm`, e importa los módulos de prueba. Aquí hay una prueba simple que verifica una función `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Una función de suma simple"
        [ test "Sumar 2 y 3 da como resultado 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Para ejecutar tus pruebas, necesitarás `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Esto compilará tus pruebas e imprimirá los resultados en tu terminal. Para el ejemplo anterior, la salida debería ser algo así como:

```
LA EJECUCIÓN DE LA PRUEBA PASÓ

Duración: 42 ms
Pasadas:   1
Fallidas:   0
```

Para un ejemplo más complejo, digamos que quieres realizar una prueba de fuzz en la función `add` para asegurarte de que maneja correctamente una amplia gama de entradas enteras. Modificarías tu `ExampleTest.elm` de la siguiente manera:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Probar add con fuzzing"
        [ fuzz int "Prueba de fuzz en add con enteros aleatorios" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Ejecuta `elm-test` nuevamente para ver las pruebas de fuzz en acción. La salida variará con entradas aleatorias pero las pruebas exitosas indicarán que no hay fallos:

```
LA EJECUCIÓN DE LA PRUEBA PASÓ

Duración: 183 ms
Pasadas:   100
Fallidas:   0
```

Estos ejemplos muestran cómo escribir y ejecutar pruebas unitarias y de fuzz simples en Elm, utilizando el paquete `elm-explorations/test`. Las pruebas son una parte vital del proceso de desarrollo, ayudando a asegurar que tus aplicaciones Elm sean confiables y mantengan una alta calidad.
