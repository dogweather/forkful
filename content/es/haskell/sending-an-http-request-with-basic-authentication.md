---
title:                "Haskell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Hay varias situaciones en las que es necesario enviar una solicitud HTTP con autenticación básica. Algunos ejemplos comunes incluyen acceder a una API privada o realizar pruebas de integración con un servidor que requiere autenticación. 

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Haskell, primero debemos importar el módulo "Network.HTTP.Simple". A continuación, utilizando la función "setRequestMethod", podemos especificar el método de solicitud (en este caso, será "GET") y la URL a la que queremos enviar la solicitud. Luego, utilizamos la función "setRequestBasicAuth" para agregar las credenciales de autenticación básica, que consisten en el nombre de usuario y la contraseña. Finalmente, llamamos a la función "httpBS" para realizar la solicitud y recibir una respuesta.

```Haskell
import Network.HTTP.Simple

main = do
    response <- setRequestMethod "GET" "https://www.example.com"
        >>= setRequestBasicAuth "username" "password"
        >>= httpBS
    print (getResponseBody response)
```

La respuesta devuelta será un objeto "Response" que contiene la información de estado de la solicitud y el cuerpo de la respuesta. Podemos acceder al cuerpo de la respuesta utilizando la función "getResponseBody" y luego procesar los datos según sea necesario.

## Profundizando

En la autenticación básica, las credenciales (nombre de usuario y contraseña) se codifican en Base64 antes de enviarse como encabezados de autorización en la solicitud HTTP. Esto significa que es importante tener en cuenta que la autenticación básica no encripta ni protege de ninguna manera las credenciales durante el proceso de transmisión. Por lo tanto, se recomienda utilizar una conexión segura (HTTPS) al enviar una solicitud con autenticación básica para proteger la información confidencial.

## Ver también

- [Documentación de Network.HTTP.Simple](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)
- [RFC de Autenticación Básica en HTTP](https://tools.ietf.org/html/rfc7617)
- [Tutorial de Haskell en español](https://es.wikibooks.org/wiki/Haskell)