---
date: 2024-01-20 18:01:55.842721-07:00
description: "How to: Per eseguire una richiesta HTTP con autenticazione di base in\
  \ Haskell, utilizziamo la libreria `http-conduit`. Installala con `cabal install\
  \ http-\u2026"
lastmod: '2024-03-13T22:44:43.474785-06:00'
model: gpt-4-1106-preview
summary: Per eseguire una richiesta HTTP con autenticazione di base in Haskell, utilizziamo
  la libreria `http-conduit`.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## How to:
Per eseguire una richiesta HTTP con autenticazione di base in Haskell, utilizziamo la libreria `http-conduit`. Installala con `cabal install http-conduit`.

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let username = "utente"
  let password = "secret"
  let auth = BS.concat ["Basic ", encode (BS.concat [username, ":", password])]
  
  request' <- parseRequest "http://esempio.com/dati"
  let request = setRequestHeader hAuthorization [auth] request'

  response <- httpLBS request
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  print $ getResponseBody response
```

Esegui e vedi il risultato, che includerà il codice di stato e il contenuto della risposta.

## Deep Dive
L'autenticazione HTTP di base esiste dagli inizi del web. Sebbene non sia la più sicura (le credenziali sono codificate in Base64 ma non criptate), è semplice da implementare. Oggi, sarebbe meglio usare alternative come l'autenticazione OAuth. In Haskell, le richieste HTTP sfruttano le librerie come `http-conduit`, che gestiscono dettagli basso-livello come la connessione di rete e l'elaborazione delle risposte.

## See Also
- `http-conduit` documentation: [https://hackage.haskell.org/package/http-conduit](https://hackage.haskell.org/package/http-conduit)
- HTTP Basic Authentication on MDN: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- Alternative Auth Methods – OAuth: [https://oauth.net/](https://oauth.net/)
