---
title:                "Elm: Leyendo argumentos de línea de comandos"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Leer argumentos de línea de comandos puede ser una habilidad muy valiosa para cualquier programador de Elm. Esto permite que los programas interactúen con el usuario de una manera más dinámica y personalizada, mejorando la experiencia del usuario final.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Elm, debemos utilizar la función `Html.programWithFlags` y pasarle una función encargada de manejar dichos argumentos. Veamos un ejemplo:

```elm
module Main exposing (..)

import Html exposing (text)
import Html.App as Html
import Html.Events exposing (onInput)

main =
  Html.programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { name : String }

init : (Model, Cmd msg)
init =
  (Model "", Cmd.none)

type Msg
  = Update String

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Update name ->
      ( { model | name = name }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Html Msg
view model = 
  text ("Hello " ++ model.name)

```

El ejemplo anterior nos muestra cómo inicializar, actualizar y visualizar una aplicación que recibe argumentos de línea de comandos. La función `init` establece un modelo con un campo `name` de tipo `String`, el cual será utilizado para almacenar el argumento pasado por el usuario. La función `update` se encarga de actualizar el modelo de acuerdo al mensaje recibido (en este caso, el valor del argumento). Y finalmente, la función `view` muestra un saludo personalizado utilizando el valor del argumento en el modelo.

Para probar este ejemplo, podemos ejecutar el siguiente comando en la terminal:

`elm make Main.elm --output=app.js`

Luego, podemos ejecutar el archivo JavaScript generado y pasarle un argumento utilizando el comando:

`node app.js "Juan"`

Esto debería mostrar en la terminal el mensaje "Hello Juan".

## Profundizando

Ahora que sabemos cómo leer argumentos de línea de comandos en Elm, podemos explorar más a fondo cómo utilizar esta funcionalidad en nuestras aplicaciones. Algunas ideas para probar podrían ser: utilizar múltiples argumentos, validar los argumentos recibidos, y utilizar la librería `elm/parser` para parsear argumentos más complejos.

## Ver también

- [Documentación oficial de Elm sobre command line arguments](https://guide.elm-lang.org/interop/command_line.html)
- [Ejemplo de lectura de argumentos de línea de comandos de Elm en GitHub](https://github.com/elm/compiler/blob/2cc270265444ff0a77278970cdea2ae3dc18c8eb/hints/report-node-version.elm#L19)
- [Tutorial de Elm sobre cómo leer y validar argumentos de línea de comandos](https://elmprogramming.com/elm-command-line-args.html)