---
date: 2024-01-20 15:14:01.855312-07:00
description: "C\xF3mo hacerlo: Hist\xF3ricamente, obtener la fecha en programaci\xF3\
  n ha estado vinculado al sistema operativo del host. En Elm, se usa el m\xF3dulo\
  \ `Time` para\u2026"
lastmod: '2024-04-05T22:51:12.749003-06:00'
model: unknown
summary: "Hist\xF3ricamente, obtener la fecha en programaci\xF3n ha estado vinculado\
  \ al sistema operativo del host."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Task
import Time exposing (Posix)

type alias Model = Posix

type Msg = UpdateTime Posix

-- Iniciar la aplicación y suscribirse a cambios en el tiempo
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- Inicializar el modelo con un tiempo en cero
init : () -> (Model, Cmd Msg)
init _ =
    (Time.millisToPosix 0, Task.perform UpdateTime Time.now)

-- Actualizar el modelo con el nuevo tiempo
update : Msg -> Model -> (Model, Cmd Msg)
update (UpdateTime newTime) _ =
    (newTime, Cmd.none)

-- Mostrar la fecha actual
view : Model -> Html Msg
view currentTime =
    text (String.fromInt (Time.posixToMillis currentTime))

-- Suscribirse a Time.every para recibir ticks del reloj
subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (UpdateTime)

```

Sample output:
```
1616265392390
```

## Profundización
Históricamente, obtener la fecha en programación ha estado vinculado al sistema operativo del host. En Elm, se usa el módulo `Time` para interactuar con fechas y horas. Alternativas incluyen utilizar bibliotecas de terceros para funcionalidades más complejas. Elm trata la hora como Posix, que representa milisegundos desde la época Unix (1 de enero de 1970). Se prefiere usar `Cmd Msg` para mantener las actualizaciones dentro del arquitectura de Elm.

## Ver También
- [Documentación oficial de Elm - Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Guide - Effects](https://guide.elm-lang.org/effects/)
- [Paquete `justinmimbs/time-extra` para funciones adicionales de tiempo](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/)
