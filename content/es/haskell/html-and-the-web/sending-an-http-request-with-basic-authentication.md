---
title:                "Enviando una solicitud http con autenticación básica"
aliases: - /es/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:00.518598-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica implica incluir credenciales de usuario (usuario y contraseña) en una solicitud para acceder a recursos protegidos en un servidor. Los programadores lo hacen para interactuar con APIs o webs que requieren autenticización segura.

## Cómo Hacerlo:

Puedes usar `http-client` y `http-client-tls` para hacer solicitudes seguras.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  
  let auth = "user:password"
      authEncoded = encode (pack auth)
      headers = [(hAuthorization, "Basic " <> authEncoded)]
      
  request <- parseRequest "https://your-api-here.com/resource"
  let requestWithAuth = setRequestHeaders headers request

  response <- httpLbs requestWithAuth manager
  putStrLn $ "Response status code: " ++ (show . statusCode . responseStatus $ response)
  print $ responseBody response
```

Sample output:

```
Response status code: 200
"{\"data\":\"Some protected content\"}"
```

## Profundizando:

La autenticación básica es un estándar de HTTP remontándose a los primeros días del web. Aunque ya no es la forma más segura de autenticación debido a que las credenciales van codificadas en Base64 (no cifradas), viene in handy para la comunicación entre servidores confiables o cuando se utiliza sobre HTTPS.

Alternativas más seguras incluyen OAuth y JWT. Pero para pruebas rápidas o entornos controlados, la autenticación básica es simple y directa.

Implementación en Haskell es sencilla gracias a bibliotecas como `http-client` que manejan las conexiones y autenticación de forma abstracta, permitiendo que te concentres en la lógica de tu aplicación.

## Vea También:

- Documentación de `http-client`: http://hackage.haskell.org/package/http-client
- Documentación de `http-client-tls`: http://hackage.haskell.org/package/http-client-tls
- RFC 7617 – The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Mejores prácticas de autenticación HTTP: https://owasp.org/www-community/controls/REST_Security_Cheat_Sheet
