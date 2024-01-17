---
title:                "Inviare una richiesta http"
html_title:           "Haskell: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Mandare una richiesta HTTP significa inviare una richiesta da un client ad un server per ottenere informazioni o eseguire un'azione. I programmatori utilizzano questa tecnologia per scambiare dati tra diverse applicazioni, siti web o dispositivi.

## Come fare:
Esempi di codice e output nella sezione ```Haskell ...```
Per inviare una richiesta HTTP nella versione attuale di Haskell, è possibile utilizzare la libreria "Network.HTTP.Simple". Questa libreria fornisce funzioni per creare e inviare richieste HTTPS, nonché per gestire la risposta del server. Di seguito un esempio di codice per effettuare una richiesta GET e ottenere il contenuto della pagina web richiesta.

```Haskell let request = setRequestMethod methodGet $ setRequestPath path defaultRequest 
let response = httpLbs request
print $ getResponseBody response 
```

L'output del codice sarà il contenuto della pagina web ottenuta come una stringa.

## Approfondimenti:
Le richieste HTTP sono stati introdotte nel 1991 come parte del protocollo HTTP 0.9. Oggi, la versione più utilizzata di HTTP è la 1.1, che supporta la persistenza della connessione e il contenuto compresso. È possibile inviare richieste HTTP utilizzando altre libreria di Haskell, come ad esempio "Network.HTTP.Conduit".

## Vedi anche:
- [Network.HTTP.Simple documentation](http://hackage.haskell.org/package/http-client)
- [Network.HTTP.Conduit documentation](http://hackage.haskell.org/package/http-conduit)
- [Introduzione a HTTP](https://developer.mozilla.org/it/docs/Web/HTTP/Overview)