---
title:                "Enviando una solicitud http"
html_title:           "Haskell: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP es una de las tareas más comunes en la programación web. Puede permitir a los desarrolladores establecer comunicación entre su aplicación y servidores externos, lo que le da a su aplicación más funcionalidad y flexibilidad.

## Cómo hacerlo

Para enviar una solicitud HTTP en Haskell, necesitamos utilizar la biblioteca `http-client` y `http-conduit`. Aquí hay un ejemplo de cómo hacer una solicitud GET utilizando `http-conduit`:

```Haskell
import Network.HTTP.Conduit -- importamos la biblioteca

-- Creamos un Manager que maneje nuestra conexión HTTP
manager <- newManager tlsManagerSettings

-- Creamos un objeto Request con la URL deseada
request <- parseRequest "http://www.ejemplo.com"

-- Utilizamos `httpLbs` para realizar la solicitud GET
response <- httpLbs request manager

-- Imprimimos el cuerpo de la respuesta
putStrLn $ "El cuerpo es: " ++ responseBody response 
```

La salida del ejemplo anterior sería el contenido de la página web solicitada.

## Profundizando

Existen diferentes tipos de solicitudes HTTP, como GET, POST, PUT, DELETE, etc. Además, también podemos enviar datos en nuestra solicitud, como parámetros de consulta o JSON en el cuerpo de la solicitud. Para eso, necesitaremos utilizar funciones adicionales de la biblioteca `http-conduit` como `responseBody`, `responseHeaders` y `httpRequest`.

## Vea también

- Documentación de `http-client`: https://hackage.haskell.org/package/http-client
- Documentación de `http-conduit`: https://hackage.haskell.org/package/http-conduit
- Ejemplos de solicitudes HTTP en Haskell: https://www.parsonsmatt.org/2015/05/08/yesod_kitchensink.html