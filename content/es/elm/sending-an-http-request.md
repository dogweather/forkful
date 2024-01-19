---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Enviar un HTTP request es un método para obtener o enviar información a través de internet. Como programadores, hacemos tal solicitud principalmente para interactuar con servidores web y APIs, ya sea para pedir datos, enviar datos, o ambos.

## Cómo hacerlo:

Vamos a enviar una solicitud HTTP GET en Elm para solicitar información de una API:

```Elm
import Http
import Json.Decode as Decode

type Msg = GotData (Result Http.Error String)

getData : Cmd Msg
getData =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectString GotData
        }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Ok data) ->
            ( data, Cmd.none )

        GotData (Err _) ->
            ( model, Cmd.none )
```
En este código, estamos obtiendo datos del URL proporcionado y esperamos una cadena para poder manejarla posteriormente. 

## Profundización

Históricamente, con la salida de Elm 0.19, la biblioteca Http ha sido revisada para un mejor manejo de errores y una mayor flexibilidad en la configuración de las solicitudes.

Alternativas para enviar Http request podrían ser `fetch()` en JavaScript, `curl` en las líneas de comando de Unix, etc. No obstante, Elm proporciona una manera segura y fuertemente tipada para hacerlo.

Sobre los detalles de implementación, notarás que la función `Http.get` toma un registro con `url` y `expect`. `url` es una cadena que indica a dónde quieres enviar la solicitud. `expect` le indica a Elm qué tipo de datos esperas recibir, en este caso, esperamos una cadena.

## Ver También

Para una guía más completa sobre solicitudes HTTP en Elm, puedes visitar la documentación oficial aquí: https://package.elm-lang.org/packages/elm/http/latest/

También puedes ver el paquete Json.Decode para obtener más información sobre cómo descomponer los datos recibidos de una solicitud HTTP: https://package.elm-lang.org/packages/elm/json/latest/Decode