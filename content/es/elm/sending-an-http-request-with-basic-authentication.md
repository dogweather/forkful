---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Enviar una solicitud HTTP con autenticación básica implica anexar credenciales codificadas en base64 en el encabezado de una solicitud HTTP. Los programadores lo hacen para asegurar la comunicación entre el cliente y el servidor, restringiendo el acceso a los recursos sólo a los usuarios autenticados.

## Cómo hacerlo:

Para enviar una solicitud HTTP con autenticación básica en Elm, utilizaremos el paquete Http. A continuación, algunos bloques de código.

```Elm
import Http
import Base64

autenticacionBasica : String -> String -> Http.Header
autenticacionBasica usuario contraseña =
    let
        credenciales =
            Base64.encode (usuario ++ ":" ++ contraseña)
    in
    Http.header "Authorization" ("Basic " ++ credenciales)
```

Efectivamente, nombre de usuario y contraseña se combinan en un string, se codifican en base64, y luego se añaden al encabezado de la solicitud HTTP.

```Elm
import Json.Decode as Decode

solicitarRecursos : Http.Request String
solicitarRecursos =
    Http.request
        { method = "GET"
        , headers = [ autenticacionBasica "usuario" "contraseña" ]
        , url = "https://api.miservidor.com/resource"
        , body = Http.emptyBody
        , expect = Http.expectString Decode.string
        , timeout = Nothing
        , tracker = Nothing
        }
```

Nuestro método de autenticación ahora se utiliza en una solicitud HTTP GET a nuestro endpoint.

## Profundización:

Historicamente, la autenticación básica fue una de las primeras maneras estándar de manejar la autenticación en HTTP. Aunque todavía se utiliza hoy en día, tiene limitaciones y existen alternativas mejores y más seguras como la autenticación de token o la autenticación OAuth.

Implementar la autenticación básica en Elm es bastante sencillo gracias a las funciones del paquete Http, pero el uso en producción debe ser analizado cuidadosamente debido a sus problemas de seguridad conocidos. Las credenciales codificadas en base64 pueden ser fácilmente descifradas, por lo que la autenticación básica debe ser usada sólo sobre conexiones seguras HTTPS.

## Ver También:

1. Documentación del paquete Elm Http: https://package.elm-lang.org/packages/elm/http/latest/.
2. Autenticación básica en la MDN web docs: https://developer.mozilla.org/es/docs/Web/HTTP/Authentication.
3. Documentación Base64 en Elm: https://package.elm-lang.org/packages/truqu/elm-base64/latest/. 
4. Autenticación en la API de Elm: http://package.elm-lang.org/packages/elm-lang/http/latest/Http#expectString.
5. Especificaciones de autenticación básica RFC7617: https://tools.ietf.org/html/rfc7617.