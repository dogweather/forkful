---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Elm: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica en Elm?

Enviar una solicitud HTTP con autenticación básica en Elm es una forma de asegurar la comunicación entre un cliente y un servidor web. Esto es especialmente importante cuando se manejan datos sensibles, como información personal o bancaria, ya que la autenticación básica proporciona una capa adicional de seguridad al requerir un nombre de usuario y una contraseña para acceder a la información.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Elm, primero se debe importar el módulo `Http` y el módulo `Basicauth` de la biblioteca `elm/http`.

```Elm
import Http
import Basicauth
```

Luego, se debe crear una función que tome como argumentos el nombre de usuario y la contraseña para generar la cadena de autenticación.

```Elm
basicAuth : String -> String -> String
basicAuth username password =
    "Basic " ++ (Basicauth.encode username password)
```

A continuación, se debe definir la función `sendRequest` que se encargará de enviar la solicitud HTTP con la autenticación básica incluida en el encabezado de la solicitud.

```Elm
sendRequest : String -> String -> Cmd Msg
sendRequest username password =
    let
        authString =
            basicAuth username password

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" authString ]
                , url = "https://myapi.com"
                , body = Http.emptyBody
                , expect = Http.expectJson GotData MyDataDecoder
                }
    in
        Http.send GotData request
```

Finalmente, se invoca la función `sendRequest` con el nombre de usuario y la contraseña deseados para enviar la solicitud HTTP.

```Elm
sendRequest "miusuario" "micontraseña"
```

## Profundizando

Al enviar una solicitud HTTP con autenticación básica en Elm, primero se crea una cadena de autenticación que sigue el formato "Basic <credenciales codificadas en base64>" utilizando la función `Basicauth.encode`. Luego, se incluye esta cadena en el encabezado de la solicitud utilizando `Http.header` y se envía la solicitud utilizando `Http.send`. Es importante tener en cuenta que la autenticación básica no proporciona una seguridad absoluta y se recomienda utilizar otros métodos de autenticación más seguros para proteger los datos sensibles.

## Ver también

- [Documentación de la biblioteca Http en Elm](https://package.elm-lang.org/packages/elm/http/latest/)
- [Documentación de la biblioteca Basicauth en Elm](https://package.elm-lang.org/packages/NoRedInk/elm-basic-auth/latest/)