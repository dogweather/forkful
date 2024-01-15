---
title:                "Descargar una página web"
html_title:           "Haskell: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en programación funcional y quieres aprender un lenguaje versátil y eficiente, entonces Haskell es perfecto para ti. Además, con la creciente importancia de la web, saber cómo descargar páginas web puede ser útil para proyectos personales o profesionales.

## Cómo hacerlo

En Haskell, puedes descargar una página web utilizando la biblioteca `Network.HTTP.Simple`. Primero, importamos la biblioteca en nuestro archivo con `import Network.HTTP.Simple`.

A continuación, creamos una función que tomará una URL como entrada y devolverá el contenido de la página web en formato de texto plano:

```Haskell
getPage :: String -> IO String
getPage url = do
    request <- parseRequest url
    response <- httpLBS request
    return $ getResponseBody response
```

Con la función `getPage`, podemos descargar el contenido de una página web y almacenarlo en una variable. Por ejemplo, si queremos descargar la página principal de Google, podemos hacerlo de la siguiente manera:

```Haskell
main :: IO ()
main = do
    page <- getPage "https://www.google.com/"
    print page
```

El resultado de imprimir `page` sería el código HTML de la página de inicio de Google en texto plano.

## Inmersión profunda

La función `getPage` que creamos utiliza dos funciones de la biblioteca `Network.HTTP.Simple`: `parseRequest` y `httpLBS`. La primera es para crear una solicitud HTTP a partir de la URL que se le pasa como argumento, mientras que la segunda envía esa solicitud y devuelve la respuesta.

También es importante tener en cuenta que `getPage` devuelve un valor de tipo `IO String`, lo que significa que es una acción de entrada y salida que eventualmente devuelve una cadena de texto. Esto se debe a que la descarga de una página web es una operación de entrada y salida y puede ser bloqueante, lo que significa que el programa se detendrá hasta que se complete la operación.

## Ver también

- [Documentación de la biblioteca Network.HTTP.Simple](https://hackage.haskell.org/package/http-client)
- [Más información sobre Haskell](https://www.haskell.org/)