---
title:                "Haskell: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web es una tarea común en el mundo de la programación. Ya sea para extraer información o para analizar el contenido, tener la habilidad de descargar una página web es esencial en muchas aplicaciones. Además, es una tarea relativamente sencilla de realizar en el lenguaje de programación Haskell. En este artículo, aprenderemos cómo descargar una página web utilizando Haskell y profundizaremos en cómo funciona este proceso.

## Cómo hacerlo

Para descargar una página web en Haskell, utilizaremos la librería "HTTP". Primero, debemos importarla en nuestra aplicación:

```Haskell
import Network.HTTP
```

Luego, podemos utilizar la función "simpleHTTP" para realizar una solicitud HTTP a una URL específica. Por ejemplo, si queremos descargar la página principal de Google, podemos hacer lo siguiente:

```Haskell
main :: IO ()
main = do
    response <- simpleHTTP (getRequest "https://www.google.com")
    content <- getResponseBody response
    putStrLn content
```

En este código, utilizamos la función "getRequest" para crear una solicitud HTTP a la URL de Google. Luego, utilizamos la función "getResponseBody" para obtener el contenido de la respuesta y, finalmente, lo imprimimos en la consola.

Al ejecutar este código, veremos el código fuente de la página principal de Google en la consola.

## Profundizando

Ahora que sabemos cómo descargar una página web en Haskell, vamos a profundizar y entender cómo funciona este proceso. Primero, la función "simpleHTTP" crea una solicitud HTTP y la envía al servidor especificado en la URL. El servidor luego responde con una respuesta que contiene un código de estado, encabezados y el contenido de la página.

Para obtener el contenido de la respuesta, utilizamos la función "getResponseBody", que extrae el contenido del cuerpo de la respuesta. Dependiendo del tipo de contenido que esperemos recibir, podemos utilizar otras funciones como "getResponseCode" o "getResponseHeaders" para obtener información adicional de la respuesta.

Es importante tener en cuenta que, al utilizar la función "simpleHTTP", no estamos controlando las posibles excepciones que puedan ocurrir durante el proceso de descarga. Por lo tanto, es recomendable utilizar una variante de esta función que permita el manejo de excepciones, como "simpleHTTP_".

## Ver también

- Documentación de la librería HTTP: https://hackage.haskell.org/package/HTTP
- Ejemplo de descarga de una página web utilizando la librería HTTP: https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions
- Otros métodos para descargar contenido en Haskell: https://www.stackbuilders.com/tutorials/haskell/performing-http-requests-with-haskell