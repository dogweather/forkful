---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Haskell: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# ¡Hola Programadores!

## ¿Qué y Por Qué?

Enviar una solicitud de HTTP con autenticación básica es la forma en que los programadores se aseguran de que solo usuarios autorizados pueden acceder a un recurso o servicio en línea. Es una medida de seguridad común en aplicaciones web, ya que requiere que los usuarios ingresen credenciales como nombre de usuario y contraseña antes de acceder a una página o realizar una acción.

## Cómo:

```Haskell
import Network.HTTP
import Network.HTTP.Auth
import Network.HTTP.Headers
```

Para enviar una solicitud de HTTP con autenticación básica en Haskell, necesitaremos importar los módulos necesarios mencionados anteriormente. Luego, podemos utilizar la función ```simplyHTTP``` para enviar una solicitud de GET con autenticación básica. Aquí hay un ejemplo de cómo se vería el código:

```Haskell
simplyHTTP (getRequest "www.ejemplo.com") >>= sendW ⟩> 
putStr . rspBody
```

En este ejemplo, estamos enviando una solicitud de GET al sitio web "www.ejemplo.com" con autenticación básica y luego imprimiendo la respuesta en la consola utilizando la función ```putStr```.

## Profundizando:

La autenticación básica de HTTP fue introducida en la especificación de HTTP/1.0 en 1996 como una forma simple de autenticar usuarios en aplicaciones web. Sin embargo, debido a que las credenciales se envían en texto plano, no es una medida de seguridad confiable y es vulnerable a ataques de hackers. Por lo tanto, se recomienda utilizar otras formas de autenticación más seguras, como OAuth o JSON Web Tokens (JWT).

Si quieres saber más sobre cómo implementar la autenticación básica en Haskell, te recomiendo leer más sobre las funciones ```simplyHTTP``` y ```sendW```. Estas funciones se pueden usar para enviar diferentes tipos de solicitudes HTTP y también permiten especificar encabezados de autenticación adicionales.

## Vea También:

- [Documentación de Network.HTTP] (https://hackage.haskell.org/package/HTTP-4000.3.12/docs/Network-HTTP.html)
- [Solicitudes HTTP en Haskell] (https://mmhaskell.com/blog/2017/5/9/practical-haskell-making-http-requests)
- [Biblioteca de autenticación de red de Haskell] (https://hackage.haskell.org/package/authHTTP-1.0.7/docs/src/Network.HTTP.Auth.html)