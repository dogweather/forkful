---
title:                "Enviando una solicitud http"
html_title:           "Elm: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

# ¿Por qué enviar una solicitud HTTP en Elm?

Enviar una solicitud HTTP es una forma esencial de comunicarse con servicios externos y obtener datos o realizar acciones en nuestra aplicación Elm. Aprender a hacerlo nos permitirá ampliar las capacidades de nuestras aplicaciones y aprovechar todo el potencial de la Web.

## Cómo hacerlo

En Elm, podemos enviar una solicitud HTTP utilizando la función `Http.send`. Primero, debemos importar el módulo HTTP en nuestro archivo:

```Elm
import Http
```

Luego, podemos crear una solicitud `Http.Request` pasando la URL y el método HTTP correspondiente, como GET o POST:

```Elm
request : Http.Request
request =
    Http.request
        { url = "https://my-api.com/users"
        , method = "GET"
        }
```

Podemos enviar esta solicitud utilizando la función `Http.send`, que espera un mensaje de tipo `Http.Response` y una solicitud `Http.Request`:

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...
        FetchUsers ->
            ( model, Http.send UserListReceived request )
```

Finalmente, podemos manejar la respuesta en nuestro modelo utilizando el mensaje `Http.Response`, que contendrá un estado, encabezados y posiblemente un cuerpo de respuesta:

```Elm
type User
    = User
        { id : Int
        , name : String
        }

type alias Model
    = { users : List User
      , loading : Bool
      }

type Msg
    = ...
    | UserListReceived (Result Http.Error (List User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ...
        UserListReceived (Ok users) ->
            ( { model | users = users, loading = False }, Cmd.none )
        
        UserListReceived (Err error) ->
            ( model, Cmd.none )
```

## Profundizando

En la solicitud `Http.request`, también podemos incluir un cuerpo y encabezados adicionales, si es necesario. Además, podemos utilizar la función `Http.toBuilder` para construir una solicitud más avanzada con opciones personalizadas.

Además, con la biblioteca adicional [elm-lang/http](https://package.elm-lang.org/packages/elm-lang/http/latest/), podemos manejar errores y autenticación en nuestras solicitudes HTTP, así como configurar un manejo de intercepción para modificar las solicitudes antes de ser enviadas.

# Ver también

- [Documentación oficial de Elm para enviar solicitudes HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm for Beginners: Sending HTTP Requests](https://www.elm-tutorial.org/en/08-fetching-resources/02-request.html)
- [Biblioteca elm-lang/http](https://package.elm-lang.org/packages/elm-lang/http/latest/)