---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Invio di una richiesta HTTP è un metodo per ottenere o inviare dati da/a un server. I programmatori lo fanno per interagire con servizi web, scaricare file, ottenere dati JSON, ecc.

## Come fare:
Per inviare una richiesta HTTP in Haskell, usiamo il pacchetto `http-client`. Ecco un esempio semplice per eseguire una richiesta GET.

```Haskell
import Network.HTTP.Client 
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings

    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager

    putStrLn $ "Il codice di stato HTTP era: " ++ 
               (show . statusCode . responseStatus $ response)
```

Quando esegui questo codice, riceverai un output simile a `"Il codice di stato HTTP era: 200"`

## Approfondimento:
(1) Inviare richieste HTTP in Haskell non era sempre così semplice. Prima dell'introduzione del pacchetto `http-client`, era necessario utilizzare librerie di più basso livello che richiedevano molto più codice.
(2) Ci sono molte altre librerie Haskell per l'invio di richieste HTTP, come `http-conduit` e `wreq`, che forniscono un'interfaccia di alto livello per operazioni HTTP.
(3) Le richieste HTTP in Haskell vengono inviate in maniera asincrona utilizzando schede verdi o thread leggeri.

## Vedi Anche:
1. Documentazione di `http-client`: https://hackage.haskell.org/package/http-client
2. Tutorial di `http-conduit`: https://haskell-lang.org/library/http-conduit
3. Documentazione di `wreq`: https://hackage.haskell.org/package/wreq