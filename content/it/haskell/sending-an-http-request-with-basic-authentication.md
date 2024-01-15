---
title:                "Inviare una richiesta HTTP con autenticazione di base"
html_title:           "Haskell: Inviare una richiesta HTTP con autenticazione di base"
simple_title:         "Inviare una richiesta HTTP con autenticazione di base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Hai mai bisogno di accedere ad un sito web o ad un API che richiede autenticazione di base tramite una richiesta HTTP? Se sì, allora sei nel posto giusto! In questo articolo impareremo come inviare una richiesta HTTP con autenticazione di base utilizzando Haskell. E se non hai mai avuto bisogno di farlo, leggi comunque questo articolo perché potrebbe rivelarsi utile in futuro.

## Come fare

È molto semplice inviare una richiesta HTTP con autenticazione di base utilizzando Haskell. Prima di tutto, dobbiamo importare il modulo "Network.HTTP.Simple" per poter utilizzare le funzioni necessarie. Dopodiché, dobbiamo fornire un URL valido e le credenziali dell'utente (in questo esempio utilizzeremo un sito API fittizio).

```Haskell
import Network.HTTP.Simple

request <- setRequestBasicAuth "username" "password" "http://www.exampleapi.com"

response <- httpBS request

print (getResponseBody response)
```

L'output dovrebbe essere una stringa contenente i dati richiesti dall'API.

## Approfondimento

Ora che abbiamo visto come inviare una richiesta HTTP con autenticazione di base in Haskell, possiamo esaminare più a fondo cosa succede dietro le quinte. Quando inviamo una richiesta con autenticazione di base, dobbiamo fornire un nome utente e una password per ottenere l'accesso alle risorse protette. Queste credenziali vengono codificate in base64 e poi inviate al server nel campo di autorizzazione dell'header della richiesta HTTP. Il server decodificherà quindi le credenziali e verificherà se l'utente ha il permesso di accedere alle risorse richieste.

## Vedi anche

- [Documentazione ufficiale di Network.HTTP.Simple](https://hackage.haskell.org/package/http-client)
- [Tutorial su come utilizzare autenticazione di base in Haskell](https://www.fpcomplete.com/blog/2017/07/using-basic-auth-haskell)
- [Esempio di invio di una richiesta HTTP con autenticazione di base in Haskell](https://www.haskellforall.com/2017/09/making-http-requests-in-haskell.html)