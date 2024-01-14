---
title:                "Haskell: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

In questo post, parleremo di come inviare una richiesta HTTP utilizzando Haskell. Ciò può essere utile in molte situazioni, come ad esempio per l'interazione con API esterne o per creare applicazioni web. 

## Come fare

Il modo più semplice per inviare una richiesta HTTP in Haskell è utilizzare la libreria "http-conduit". Vediamo un esempio di codice per inviare una richiesta GET e ottenere il contenuto di una pagina web:

```Haskell
import Network.HTTP.Conduit

main = do
    -- Effettua una richiesta GET all'indirizzo URL
    response <- simpleHttp "https://example.com"
    -- Ottieni il corpo della risposta come stringa
    let body = responseBody response
    -- Stampa il contenuto della risposta
    print body
```

In questo esempio, abbiamo utilizzato la funzione "simpleHttp" che prende come argomento un URL e restituisce un oggetto "Response". Possiamo quindi accedere al corpo della risposta utilizzando "responseBody". 

Se vogliamo personalizzare la nostra richiesta, possiamo utilizzare la funzione "parseUrl" che ci permette di definire parametri come il metodo di richiesta, le intestazioni e i dati da inviare. Ecco un esempio:

```Haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8

main = do
    -- Crea un oggetto "Request"
    request <- parseUrl "https://example.com"
    -- Imposta il metodo di richiesta a POST
    let request' = request { method = "POST" }
    -- Definiamo il corpo della richiesta
    let body = L8.pack "Hello World!"
    -- Aggiungiamo il corpo alla richiesta
    let request'' = request' { requestBody = RequestBodyLBS body }
    -- Inviamo la richiesta e otteniamo la risposta
    response <- withManager $ httpLbs request''
    -- Stampa il codice di stato HTTP
    print $ responseStatus response
```

In questo esempio, abbiamo utilizzato la libreria "Data.ByteString.Lazy.Char8" per trasformare una stringa in un tipo di dati compatibile con la richiesta. Possiamo anche modificare facilmente altri parametri della richiesta, come ad esempio le intestazioni, utilizzando la sintassi mostrata sopra.

## Approfondimento

Per saperne di più sulle richieste HTTP in Haskell, è possibile consultare la documentazione ufficiale della libreria "http-conduit" e la pagina di riferimento su Hackage. Inoltre, esistono altre librerie che forniscono funzionalità simili, come "HTTP" e "wreq". Esplorare queste librerie può aiutare a trovare la soluzione migliore per le proprie esigenze.

## Vedi anche
- [Documentazione della libreria `http-conduit`](https://hackage.haskell.org/package/http-conduit/docs/Network-HTTP-Conduit.html)
- [Pagina di riferimento su Hackage](https://hackage.haskell.org/package/http-conduit)
- [Esempio di codice su GitHub](https://github.com/snoyberg/http-conduit#example-usage)