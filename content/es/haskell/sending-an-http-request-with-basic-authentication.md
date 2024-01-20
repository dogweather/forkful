---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando una solicitud HTTP con autenticación básica en Haskell

## ¿Qué y por qué?
Enviar una solicitud HTTP con autenticación básica es un método para acceder a recursos de un servidor que requieren un usuario y una contraseña. Los programadores lo hacen para interactuar con APIs protegidas.

## ¿Cómo hacerlo?
Para enviar una solicitud HTTP con autenticación básica en Haskell, vamos a utilizar la biblioteca `http-conduit`. Necesitarás instalarla con `cabal install http-conduit`. Aquí hay un código de muestra:

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Client (applyBasicAuth)

main :: IO ()
main = do
    let request = setRequestPath "/resource" $
                  setRequestHost "www.example.com" $
                  setRequestPort 443 $
                  setRequestSecure True $ 
                  applyBasicAuth "username" "password" $
                  defaultRequest

    response <- httpLBS request
    putStrLn $ "Status: " ++ show (getResponseStatusCode response)
    print $ getResponseBody response
```

Este programa hace una petición a `www.example.com/resource` con autenticación básica y luego imprime el estado de la respuesta y el cuerpo de la respuesta.

## Profundizando
La autenticación básica fue parte de la especificación original de HTTP creada en 1992. Envía nombre de usuario y contraseña en claro (base64) sobre la red, por lo que por razones de seguridad, siempre debe usarse junto con HTTPS, que cifra el tráfico.

Una alternativa a la autenticación básica es la autenticación de portador de token, a menudo utilizada junto con OAuth 2.0. En lugar de enviar un nombre de usuario y una contraseña, envías un token, que puede ser revocado en cualquier momento.

La función `applyBasicAuth` en `http-conduit` toma un nombre de usuario y una contraseña, los codifica en Base64, y los pone en el encabezado de autorización. ¿Quieres ver los detalles detallados? Observa el código fuente de la función: https://hackage.haskell.org/package/http-client-0.7.3/docs/src/Network-HTTP-Client.html#applyBasicAuth

## Ver también
- Documentación de http-conduit: https://hackage.haskell.org/package/http-conduit
- Especificación original de HTTP: https://www.w3.org/Protocols/HTTP/AsImplemented.html
- Autenticación Bearer Token: https://tools.ietf.org/html/rfc6750
- Detalles sobre base64: https://en.wikipedia.org/wiki/Base64