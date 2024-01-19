---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio Richiesta HTTP con Autenticazione Basilare in Haskell

## Che cos'è e perché?

L'invio di una richiesta HTTP con autenticazione basica è un processo in cui un client manda una richiesta al server con le credenziali dell'utente codificate in base64. È un metodo comune per garantire la sicurezza e l'autorizzazione nelle applicazioni web.

## Come fare:

Per inviare una richiesta HTTP con autenticazione basica in Haskell, puoi usare la libreria `http-conduit`. Installare il pacchetto con il comando `cabal update && cabal install http-conduit`

Ecco un esempio di codice:

```Haskell
import Network.HTTP.Simple
import Network.HTTP.Types.Header

main :: IO ()
main = do
  let request' = setRequestBasicAuth "username" "password"
                $ "http://example.com"
  response <- httpLBS request'

  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  LBS.putStrLn $ getResponseBody response
```

In questo snippet, stiamo impostando un nome utente e una password sulla richiesta al server. Alla fine, stamperemo il codice di stato, l'intestazione "Content-Type" e il corpo della risposta.

## Approfondimento

L'autenticazione HTTP basica è stata introdotta con la specifica HTTP/1.0 negli anni '90 come metodo semplice e diretto per controllare l'accesso.

Esistono alternative all'autenticazione basica, come l'autenticazione digest o l'autenticazione token-based. L'autenticazione basica è tuttavia la più semplice da implementare nonostante sia la meno sicura.

L'autenticazione basica in Haskell può essere realizzata attraverso la libreria `http-conduit`, che offre un'API di alto livello per l'invio di richieste HTTP.

## Vedi Anche

- Documentazione di http-conduit: http://hackage.haskell.org/package/http-conduit
- Guida alla sicurezza di HTTP: https://developer.mozilla.org/it/docs/Web/HTTP/Authentication
- Base64 Encoding: https://www.base64encode.net/base64-url-safe-encoding