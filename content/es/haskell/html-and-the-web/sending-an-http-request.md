---
date: 2024-01-20 17:59:54.985795-07:00
description: "C\xF3mo Hacerlo: Haskell ofrece varias bibliotecas para gestionar HTTP.\
  \ Vamos a usar `http-conduit` por su simplicidad y potencia. Primero, inst\xE1lalo\
  \ con."
lastmod: '2024-03-13T22:44:59.116008-06:00'
model: gpt-4-1106-preview
summary: Haskell ofrece varias bibliotecas para gestionar HTTP.
title: Enviando una solicitud http
weight: 44
---

## Cómo Hacerlo:
Haskell ofrece varias bibliotecas para gestionar HTTP. Vamos a usar `http-conduit` por su simplicidad y potencia. Primero, instálalo con:

```shell
cabal update
cabal install http-conduit
```

Aquí hay un ejemplo simple de cómo enviar una solicitud GET:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "El status code es: " ++ show (getResponseStatusCode response)
    putStrLn $ "La respuesta es: "
    print $ getResponseBody response
```

Ejecuta y obtendrás algo parecido a:

```
El status code es: 200
La respuesta es:
"{\"args\":{},\"headers\":{\"Accept-Encoding\":\"gzip\",\"Host\":\"httpbin.org\",\"User-Agent\":\"haskell http-conduit/2.3.7.3\",\"X-Amzn-Trace-Id\":\"Root=1-5e..."}," ... (más datos aquí)
```

## Inmersión Profunda:
El protocolo HTTP (HyperText Transfer Protocol) es el fundamento de la comunicación en la web. Haskell, aunque no conocido por su soporte de IO en comparación con lenguajes como JavaScript o Python, tiene bibliotecas muy competentes para lidiar con HTTP.

`http-conduit` es parte del proyecto `conduit` que maneja el flujo de datos. Es potente y maneja automáticamente conexiones, codificación y otros aspectos tediosos.

Alternativas como `wreq` o `req` ofrecen abstracciones diferentes. Elije uno basado en tus necesidades y preferencias. Por ejemplo, `wreq` es genial para trabajar con JSON, mientras que `req` ofrece un tipo de safety atractivo con su API.

Internamente, estas bibliotecas manejan conexiones de red, protocolos de bajo nivel y representación de datos para facilitar la vida del programador.

## Ver También:
- Tutorial `http-conduit`: https://haskell-lang.org/library/http-client
- Documentación de `wreq`: http://www.serpentine.com/wreq/
- Documentación de `req`: https://hackage.haskell.org/package/req
- Guía para principiantes sobre HTTP: https://developer.mozilla.org/es/docs/Web/HTTP

Explora estos recursos para profundizar tus conocimientos en Haskell y el manejo de solicitudes HTTP. ¡Feliz codificación!
