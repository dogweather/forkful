---
title:                "Enviando una solicitud http"
aliases:
- es/elm/sending-an-http-request.md
date:                  2024-01-20T17:59:54.220058-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Mandar una petición HTTP es como enviar un mensaje a otra computadora pidiéndole información o para que realice una acción. Los programadores lo hacen para interactuar con la web: obtener datos, enviar formularios, comunicarse con servicios web, y más.

## Cómo hacerlo:

```Elm
import Http
import Json.Decode as Decode

type Msg
    = GotData (Result Http.Error String)

getData : Cmd Msg
getData =
    Http.get
        { url = "https://api.example.com/data"
        , expect = Http.expectString GotData
        }

type alias Model =
    { data : Maybe String
    , error : Maybe String
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData (Ok data) ->
            ({ model | data = Just data, error = Nothing }, Cmd.none)

        GotData (Err error) ->
            let
                errorMsg =
                    case error of
                        Http.BadUrl url -> "URL no válida: " ++ url
                        Http.Timeout -> "La petición excedió el tiempo límite."
                        Http.NetworkError -> "Error de red."
                        Http.BadStatus _ -> "Respuesta con estado no exitoso."
                        Http.BadBody msg -> "El cuerpo de la respuesta no es válido: " ++ msg
            in
            ({ model | data = Nothing, error = Just errorMsg }, Cmd.none)
```

## Profundización

El envío de solicitudes HTTP es una parte crucial de las aplicaciones web modernas. Elm maneja las peticiones HTTP de manera diferente a lenguajes como JavaScript. En Elm, todo es inmutable y gestionado por la arquitectura de comandos y suscriptores (Cmd, Sub), asegurando que los efectos secundarios, como las peticiones HTTP, estén claramente separados de la lógica de la aplicación.

Elm utiliza `Http` para realizar peticiones y `Json.Decode` para manejar la respuesta en formato JSON. Históricamente, esto ha evolucionado desde versiones más tempranas de Elm donde la gestión de efectos era menos estructurada. 

Alternativas como `Http.expectJson` permiten trabajar directamente con datos JSON si es necesario. En la implementación, Elm hace un uso intensivo de tipos y funciones puras para manejar los distintos casos de éxito y error, resultando en un código más predecible y fácil de mantener.

## Ver También

- [Elm Guide - HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm Package - HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- [Json.Decode API](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
