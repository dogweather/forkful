---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Obtener la fecha actual en la programación permite a los codificadores rastrear y registrar eventos en tiempo real. Esto es crucial en aplicaciones donde los registros precisos de fechas y horas son fundamentales, como en las operaciones de comercio electrónico o el seguimiento de proyectos.

## Cómo hacerlo:
Para obtener la fecha actual en Elm, usamos el módulo `Task` y `Time`. Vamos a crear un simple programa con Elm:

```Elm
import Browser
import Html exposing (Html, text)
import Task
import Time

-- Modelo
type alias Model =
    Maybe Time.Posix

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

-- Inicialización
init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, Task.perform NewTime Time.now )

-- Actualización
type Msg
    = NewTime Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime newTime ->
            ( Just newTime, Cmd.none )

-- Vista
view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            text "Cargando..."

        Just posix ->
            text (Time.toString posix)

-- Corre el programa
-- Verás la hora actual impresa en la pantalla
```

## Análisis más Profundo
En versiones anteriores de Elm, obtener el tiempo actual involucraba el uso de `Signal`, pero se decidió mover la funcionalidad de tiempo a `Task` y `Sub` en la versión 0.17 para simplificar la arquitectura general del idioma.

Las alternativas a la función `Time.now` incluyen el uso de otras bibliotecas de manejo de tiempo (si se necesita una funcionalidad más compleja) o realizar solicitudes HTTP a servicios de tiempo en línea.

El detalle de la implementación es que, debajo del capó, Elm recurre a la función del objeto `Date` integrado en JavaScript para obtener la fecha y hora actuales. Luego, Elm toma esta información y la convierte en un valor `Posix`, que es esencialmente un recuento de milisegundos desde la época.

## Consultar También
1. Documentación oficial de Elm sobre `Time`: [elm/time](https://package.elm-lang.org/packages/elm/time/latest/)
2. Una explicación más detallada de la función `Time.now`: [kraklin/learning-elm](https://learning-elm.github.io/07-time.html)
3. Para más preguntas sobre Elm en Español: [discord de Elm en español](https://discord.com/invite/elm-es)