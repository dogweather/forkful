---
title:                "Descargando una página web"
aliases:
- es/haskell/downloading-a-web-page.md
date:                  2024-01-20T17:44:29.824908-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qué es y por qué?

Descargar una página web significa traer el contenido de dicha página a tu ordenador o servidor. Los programadores hacen esto para analizar datos, monitorear cambios o alimentar aplicaciones con información en vivo.

## Cómo hacerlo:

Vamos a usar Haskell junto con algunas librerías prácticas. Asegúrate de tener instalados `stack` y `http-conduit`. Si no los tienes, instálalos usando `stack install http-conduit`. Aquí está el código:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    if statusCode == 200
        then putStrLn $ "Download successful! Status code: " ++ show statusCode
        else print statusCode
    print $ getResponseBody response
```

Si todo va bien, verás:

```
Download successful! Status code: 200
"<html>...</html>" -- Aquí estará el contenido de la página
```

## Análisis Profundo:

Descargar páginas web es un concepto tan antiguo como la web misma. Aunque el método básico no ha cambiado mucho, las herramientas sí. Antes se usaba `curl` o `wget` en línea de comandos, y en Haskell, librerías como `http-conduit` facilitan la tarea.

Otras opciones modernas incluyen `wreq` y `http-client`. Cada una tiene sus pros y contras. Por ejemplo, `http-conduit` es bueno para gestionar conexiones persistentes mientras que `wreq` tiene una interfaz más simple.

Detalles de implementación: en el fondo, la descarga de una página es una simple petición HTTP. Sin embargo, el manejo de errores, redirecciones y sesiones puede complicarse. Es una buena idea familiarizarse con los códigos de estado HTTP y el manejo de excepciones en Haskell.

## Ver También:

- La documentación de `http-conduit`: [https://www.stackage.org/package/http-conduit](https://www.stackage.org/package/http-conduit)
- La biblioteca `wreq` para comparar: [https://hackage.haskell.org/package/wreq](https://hackage.haskell.org/package/wreq)
- Tutorial de `http-client`: [https://hackage.haskell.org/package/http-client-tls](https://hackage.haskell.org/package/http-client-tls)
