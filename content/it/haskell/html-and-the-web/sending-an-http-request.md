---
date: 2024-01-20 17:59:38.970846-07:00
description: 'How to: In Haskell, usiamo librerie come `http-conduit` per mandare
  richieste HTTP. Ecco un esempio veloce.'
lastmod: '2024-03-13T22:44:43.472003-06:00'
model: gpt-4-1106-preview
summary: In Haskell, usiamo librerie come `http-conduit` per mandare richieste HTTP.
title: Inviare una richiesta http
weight: 44
---

## How to:
In Haskell, usiamo librerie come `http-conduit` per mandare richieste HTTP. Ecco un esempio veloce:

```haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    putStrLn $ "The body is: " ++ show (getResponseBody response)
```

Se avvii questo codice, otterrai qualcosa del tipo:

```
The status code was: 200
["application/json"]
The body is: "<qui ci sarà il corpo della risposta>"
```

## Deep Dive
Haskell, bello e funzionale, non è stato il primo a fare richieste HTTP. Questo esiste da quando internet ha avuto bisogno di comunicare a distanza. Le alternative includono librerie come `Network.HTTP` o `wget` via `System.Process`. Quando invii una richiesta, sotto il cofano, la libreria gestisce la connessione TCP/IP, costruendo l'header HTTP e interpretando la risposta.

## See Also
- La documentazione di [`http-conduit`](https://hackage.haskell.org/package/http-conduit)
- Tutorial Haskell su richieste HTTP: [School of Haskell](https://www.schoolofhaskell.com)
- Leggi sulla [`Network.HTTP`](https://hackage.haskell.org/package/HTTP) library per un'altra prospettiva
