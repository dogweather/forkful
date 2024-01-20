---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Descargar una página web implica recuperar y almacenar su contenido para su procesamiento posterior. Los programadores normalmente lo hacen para extraer datos y aplicar técnicas de web scraping.

## Cómo hacer:
Para descargar una página web en Haskell, utilizaremos el paquete extra de Haskell `http-conduit`. Asegúrate de tenerlo instalado.

```Haskell
import Network.HTTP.Conduit (simpleHttp)

descargarPagina :: String -> IO (Maybe String)
descargarPagina url = do
    contenido <- simpleHttp url
    return $ Just contenido
```

Prueba el código con una URL de prueba:

```Haskell
main :: IO ()
main = do
    let url = "http://www.example.com"
    contenido <- descargarPagina url
    print $ take 100 $ contenido
```

Un ejemplo de salida podría ser:

```Haskell
Just "<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-"
```

## Análisis en Profundidad:

1) Contexto histórico: La necesidad de descargar páginas web nació con la expansión de la World Wide Web y la creciente importancia del análisis e interpretación de los datos de la web.

2) Alternativas: En Haskell, puedes usar otros paquetes como `http-client`, `http`, `wreq`, etc. Que ofrecen funciones similares, pero pueden variar en términos de rendimiento y características adicionales.

3) Detalles de implementación: `simpleHttp` es una llamada de bloques http-conduit que intentará descargar la URL dada. Si se produce una excepción (como un error de red), fallará. Para una mayor flexibilidad, puedes usar `httpLbs`.

## Ver también:

1) `http-conduit` en Hackage: https://hackage.haskell.org/package/http-conduit
2) Tutorial de web scraping en Haskell: https://pragprog.com/titles/lotdd/learn-you-a-haskell-for-great-good/
3) Paquete `http` en Hackage: https://hackage.haskell.org/package/http