---
title:                "Haskell: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías aprender a enviar una solicitud HTTP en Haskell?

Si eres un programador de Haskell, probablemente ya sepas que este lenguaje es capaz de realizar una amplia gama de tareas. Sin embargo, si aún no has aprendido a enviar una solicitud HTTP, estás perdiendo una habilidad importante que puede ser útil en muchas situaciones. Aprender a enviar una solicitud HTTP en Haskell te permitirá comunicarte con servidores web, API's y otras aplicaciones en línea de manera eficiente.

## ¿Cómo enviar una solicitud HTTP en Haskell?

Para enviar una solicitud HTTP en Haskell, primero necesitas importar el módulo `Network.HTTP.Simple`. Luego, puedes utilizar la función `httpBS` para realizar una solicitud a una URL específica. Por ejemplo:

```Haskell
import Network.HTTP.Simple

main = do
  response <- httpBS "https://jsonplaceholder.typicode.com/posts/1"
  print $ getResponseBody response
```

Este código envía una solicitud GET al servidor JSONPlaceholder y obtiene el contenido de la respuesta como una cadena de texto. Puedes personalizar la solicitud especificando el método, encabezados, cuerpo y más. A continuación se muestra un ejemplo de cómo enviar una solicitud POST con un cuerpo JSON:

```Haskell
import Network.HTTP.Simple
import Data.Aeson

main = do
  let request = setRequestMethod "POST"
              $ setRequestHeaders [("Content-Type","application/json")]
              $ setRequestBodyJSON (object ["title" .= "Nuevo post", "body" .= "Contenido de ejemplo"])
              $ "https://jsonplaceholder.typicode.com/posts"
  
  response <- httpBS request
  print $ getResponseBody response
```

Este código envía una solicitud POST al servidor JSONPlaceholder con un cuerpo en formato JSON y obtiene la respuesta como una cadena de texto. Puedes probar diferentes combinaciones de métodos, encabezados y cuerpos para enviar diferentes tipos de solicitudes HTTP en Haskell.

## Una mirada más profunda

Enviar una solicitud HTTP en Haskell puede ser una tarea simple, pero también puedes realizar acciones más avanzadas utilizando el módulo `Network.HTTP.Client`. Este módulo te permite establecer diferentes configuraciones para tus solicitudes, manejar errores y personalizar la forma en que se procesan las respuestas. Puedes encontrar más información sobre este módulo en la documentación oficial de Haskell.

# Vea también

- [Documentación del módulo `Network.HTTP.Simple`](https://hackage.haskell.org/package/http-client-0.6.4/docs/Network-HTTP-Simple.html)
- [Documentación del módulo `Network.HTTP.Client`](https://hackage.haskell.org/package/http-client-0.6.4/docs/Network-HTTP-Client.html)
- [Tutorial: Enviar solicitudes HTTP en Haskell](https://www.michaelwestphal.de/tutorials/haskell/2016/02/12/HaskellNetworking.html)