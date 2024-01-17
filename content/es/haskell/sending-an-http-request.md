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

## ¿Qué y por qué?

Enviar una solicitud HTTP es una forma en que los programadores pueden comunicarse con servidores y obtener datos o realizar acciones en la web. Los programadores a menudo lo hacen para crear aplicaciones web y aplicaciones que usan servicios web.

## Cómo:

El envío de solicitudes HTTP es una tarea común en la programación y es posible de hacer en Haskell de forma sencilla con el paquete *http-conduit*. Primero importamos el módulo correspondiente:

```Haskell
import Network.HTTP.Simple
```

Luego, podemos hacer una solicitud GET utilizando la función `httpLBS` y proporcionando la URL que queremos consultar:

```Haskell
response <- httpLBS "https://www.example.com"
```

Podemos obtener el cuerpo de la respuesta utilizando la función `getResponseBody` y el código de estado utilizando `getResponseStatusCode`:

```Haskell
let body = getResponseBody response
let status = getResponseStatusCode response
```

Finalmente, podemos imprimir los resultados en la consola:

```Haskell
putStrLn ("Cuerpo de la respuesta: " ++ show body)
putStrLn ("Código de estado: " ++ show status)
```

La salida para esta solicitud sería:

```
Cuerpo de la respuesta: "Hola mundo!"
Código de estado: 200
```

## Profundizando:

Las solicitudes HTTP son una parte fundamental de la comunicación en la web y han sido utilizadas desde los primeros días de internet. Aunque existen alternativas como sockets y RPC, las solicitudes HTTP son más fáciles de implementar y son ampliamente utilizadas en la programación.

El paquete *http-conduit* proporciona una implementación robusta y eficiente de solicitudes HTTP en Haskell y es ampliamente utilizado por la comunidad. Además de hacer solicitudes GET, también es posible utilizar otros métodos HTTP como POST, PUT, DELETE, etc.

## Ver también:

- [Documentación del paquete *http-conduit*](https://hackage.haskell.org/package/http-conduit)
- [Tutorial de solicitudes HTTP en Haskell](https://dev.to/jhmarcus/tutorial-making-http-requests-in-haskell-5a32)