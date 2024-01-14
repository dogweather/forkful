---
title:                "Elm: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# ¿Por qué hacer solicitudes HTTP con autenticación básica en Elm?

La autenticación básica es una forma sencilla y efectiva de proteger las solicitudes HTTP en Elm. Al enviar una solicitud con autenticación básica, se agrega una capa adicional de seguridad a tu aplicación y aseguras que solo usuarios autorizados puedan acceder a la información protegida.

# Cómo hacer solicitudes HTTP con autenticación básica en Elm

Para hacer una solicitud HTTP con autenticación básica en Elm, primero necesitas importar el módulo de `Http` y el módulo de `Basicauth`. Luego, puedes usar la función `send` del módulo `Http` para enviar la solicitud con los siguientes parámetros: la URL del endpoint, el método HTTP deseado y las credenciales de autenticación que pueden ser proporcionadas por el usuario o almacenadas en variables.

```
import Http
import BasicAuth

Http.send
  { method = "GET", url = "https://miendpoint.com", expect = Http.expectString "Respuesta"}
  |> BasicAuth.withEncodedCredentials "usuario" "contraseña"
```

El módulo de `BasicAuth` también proporciona funciones para decodificar y codificar las credenciales, lo que puede ser útil si necesitas almacenarlas en alguna variable.

# Profundizando en las solicitudes HTTP con autenticación básica en Elm

Es importante tener en cuenta que la autenticación básica no es el método de autenticación más seguro, ya que envía las credenciales en texto plano a través de la red. Por lo tanto, se recomienda implementar medidas adicionales de seguridad en tu aplicación.

También es importante tener en cuenta que algunos navegadores no permiten solicitudes HTTP con autenticación básica sin conexión SSL. Por lo tanto, es importante asegurarse de que tu servidor tenga un certificado SSL válido para evitar problemas de seguridad en la autenticación básica.

# Ver también

- Módulo Http en la documentación oficial de Elm: https://package.elm-lang.org/packages/elm/http/latest/Http
- Módulo BasicAuth en la documentación oficial de Elm: https://package.elm-lang.org/packages/elm/http/latest/Http-BasicAuth