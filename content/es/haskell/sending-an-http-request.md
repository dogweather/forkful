---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

El envío de una solicitud HTTP es el proceso para solicitar datos a un servidor usando el protocolo HTTP. Los programadores lo hacen para interactuar con API's, acceder a rutas web, o recuperar datos almacenados remotamente.

## Cómo hacerlo:

Haskell cuenta con la biblioteca **http-conduit** que facilita el envío de solicitudes HTTP. Primero, necesitamos instalar dicha biblioteca utilizando el manejador de paquetes **Cabal**:

```Haskell
cabal update
cabal install http-conduit
```

Una vez instalada, podemos utilizarla de la siguiente manera:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main :: IO ()
main = do
    response <- simpleHttp "http://httpbin.org/get"
    putStrLn response
```

Esta secuencia de comandos hace una solicitud GET a http://httpbin.org/get y luego imprime la respuesta.

## Análisis profundo

El protocolo HTTP es un pilar fundamental del internet moderno, adoptado por primera vez por Tim Berners-Lee en el CERN en 1989. Haskell, a pesar de no ser conocido por su tratamiento de servicio web, tiene bibliotecas robustas y eficientes para manejar solicitudes HTTP.

Existen alternativas al paquete http-conduit en Haskell, tales como el paquete http-wreq, que proporciona una interfaz de nivel superior y es especialmente útil si requieres hacer solicitudes más complejas.

Bajo su caparazón, la biblioteca http-conduit implementa una colección de métodos simples y comprensibles para interactuar con los protocolos HTTP y HTTPS. De hecho, simpleHttp, como se usó en nuestro ejemplo, es sólo un envoltorio fácil para un proceso más complejo que cuida de gestionar conexiones, componer solicitudes y procesar respuestas de HTTP.

## Ver También

- La documentación oficial de http-conduit: https://hackage.haskell.org/package/http-conduit
- Un curso interactivo de Haskell: http://learnyouahaskell.com
- Una introducción completa al protocolo HTTP: https://developer.mozilla.org/es/docs/Web/HTTP/Overview