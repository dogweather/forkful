---
date: 2024-01-20 17:44:29.824908-07:00
description: "C\xF3mo hacerlo: Vamos a usar Haskell junto con algunas librer\xEDas\
  \ pr\xE1cticas. Aseg\xFArate de tener instalados `stack` y `http-conduit`. Si no\
  \ los tienes,\u2026"
lastmod: '2024-03-13T22:44:59.117782-06:00'
model: gpt-4-1106-preview
summary: "Vamos a usar Haskell junto con algunas librer\xEDas pr\xE1cticas."
title: "Descargando una p\xE1gina web"
weight: 42
---

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
