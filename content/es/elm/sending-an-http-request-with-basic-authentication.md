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

## ¿Qué y Por qué?
Enviar una solicitud HTTP con autenticación básica es una forma de asegurar que el servidor sólo pueda ser accedido por usuarios autorizados. Los programadores utilizan esta técnica para proteger sus aplicaciones y asegurar la privacidad de los datos.

## Cómo:

Las solicitudes HTTP con autenticación básica se pueden hacer en Elm utilizando la función `Http.send`. Se debe proporcionar un `Http.Request` que incluya las credenciales de autenticación en el encabezado de la solicitud. A continuación se muestra un ejemplo de código que ilustra cómo hacerlo:

```elm
Http.send
    ("GET", "http://mi-servidor.com/api", textDecoder)
    { headers = [ Http.header "Authorization" "Basic dXNlcm5hbWU6cGFzc3dvcmQ=" ]
    , expect = Http.expectString Respuesta
    }
``` 

En este ejemplo, se envía una solicitud HTTP GET al servidor con la URL `http://mi-servidor.com/api` y se decodifica la respuesta utilizando un `textDecoder`. El encabezado de la solicitud contiene las credenciales de autenticación codificadas en base64, con el formato `username:password`.

Una vez que la solicitud se envía, se espera una respuesta de tipo `Http.Response string`, que puede ser procesada utilizando una función `Http.expectString` y un tipo de respuesta personalizado.

## Profundizando:

La autenticación básica en HTTP fue introducida en la especificación HTTP/1.0 para proporcionar un método simple de autenticación en la web. Sin embargo, este método no es seguro y se recomienda utilizar métodos de autenticación más avanzados, como OAuth o tokens de acceso.

Una alternativa a la autenticación básica es utilizar HTTPS en lugar de HTTP, lo que asegura que todas las comunicaciones entre el cliente y el servidor estén encriptadas. Sin embargo, aún es importante tener en cuenta que la autenticación básica es vulnerable a ataques de retransmisión y puede ser fácilmente compartida o interceptada si no se utiliza HTTPS.

En términos de implementación, la autenticación básica en Elm es bastante sencilla gracias a la función `Http.send` y el uso de `Http.expectString` para decodificar la respuesta. Sin embargo, es importante tener en cuenta la seguridad y las vulnerabilidades potenciales al utilizar este método de autenticación.

## Véase también:
Para obtener más información sobre la autenticación básica en HTTP y otras formas de autenticación en la web, revisa los siguientes recursos:
- [MDN: Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [HTTP Basic authentication](https://www.elastic.co/guide/en/elasticsearch/reference/current/security-basic.html)
- [HTTP Basics Authentication Tutorial](https://www.tutorialspoint.com/http/http_basic_authentication.html)