---
title:                "Invio di una richiesta http con autenticazione di base"
html_title:           "Haskell: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Autenticazione di base in Haskell: Cos'è e Perché Utilizzarla

## Cos'è e Perché?
Invio di una richiesta HTTP con autenticazione di base è il processo di inserimento delle credenziali di accesso (nome utente e password) all'interno di una richiesta HTTP per accedere a risorse protette da restrizioni di accesso. Questo è un modo comune per autenticare le richieste di un client a un server. Solitamente viene utilizzato quando si desidera proteggere l'accesso a determinate risorse, come ad esempio un'API o un'area riservata di un sito web.

## Come fare:
Per inviare una richiesta HTTP con autenticazione di base in Haskell, è possibile utilizzare la libreria Network.HTTP.Simple. Il codice seguente mostra un esempio di come inviare una richiesta GET con autenticazione di base:

```Haskell
import Network.HTTP.Simple

sendRequest :: IO ()
sendRequest = do
  request <- parseRequest "GET http://example.com/resource"
  let auth = basicAuth "username" "password"
  response <- httpLBS (setRequestBasicAuth auth request)
  print $ getResponseBody response
```

L'esempio sopra utilizza la funzione `basicAuth` per creare un'istanza di `BasicAuth` utilizzando le credenziali fornite. Questa istanza può poi essere passata alla funzione `setRequestBasicAuth`, che a sua volta verrà utilizzata per creare una richiesta con autenticazione di base. Infine, la funzione `httpLBS` viene utilizzata per inviare la richiesta e ottenere una risposta. Il corpo della risposta viene quindi stampato a schermo.

## Deep Dive:
L'utilizzo dell'autenticazione di base è un approccio semplice ma non sicuro per proteggere l'accesso a risorse. In passato, era uno dei metodi più comuni utilizzati per autenticare richieste HTTP. Tuttavia, a causa della natura non crittografata delle credenziali inviate, è stato spesso criticato per la sua mancanza di sicurezza.

Ciò ha portato alla creazione di alternative più sicure, come ad esempio l'autenticazione a chiave pubblica (SSH). Inoltre, con l'avvento delle tecnologie di autenticazione e autorizzazione basate su token, l'autenticazione di base sta diventando sempre meno popolare.

Dal punto di vista dell'implementazione, l'autenticazione di base viene utilizzata insieme a vari protocolli di autenticazione, come ad esempio HTTP Digest o OAuth.

## Vedi anche:
- [Package Network.HTTP.Simple su Hackage](https://hackage.haskell.org/package/http-client)
- [Specifiche HTTP su w3.org](https://www.w3.org/Protocols/rfc2616/rfc2616.html)