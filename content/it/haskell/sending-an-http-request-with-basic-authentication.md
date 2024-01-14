---
title:                "Haskell: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Spesso, nel mondo della programmazione, ci troviamo ad avere la necessità di inviare una richiesta HTTP a un server per ottenere dei dati o svolgere delle azioni. In alcuni casi, per accedere ai dati o alle funzionalità del server, è necessario utilizzare delle credenziali di autenticazione di base. In questa guida, vedremo come inviare una richiesta HTTP con autenticazione di base in Haskell.

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in Haskell, dobbiamo prima importare il modulo `Network.HTTP.Simple` che ci permette di effettuare richieste HTTP. Successivamente, dobbiamo creare una richiesta utilizzando la funzione `parseRequest` specificando l'URL del server a cui vogliamo fare la richiesta. Possiamo anche specificare il tipo di metodo HTTP che vogliamo utilizzare, per esempio `GET` o `POST`. 

Una volta creata la richiesta, dobbiamo aggiungere le credenziali di autenticazione utilizzando la funzione `setRequestBasicAuth`, specificando il nome utente e la password. Infine, dobbiamo effettuare la richiesta utilizzando la funzione `httpLBS` e ottenere la risposta del server.

Di seguito, un esempio di codice che invia una richiesta GET con autenticazione di base e stampa il corpo della risposta ricevuta:

```Haskell
import Network.HTTP.Simple

main = do
	request <- parseRequest "http://www.example.com"
	response <- setRequestBasicAuth "username" "password" request >>= httpLBS
	print $ getResponseBody response
```

Risultati di esempio:

```
"Benvenuto nel sito protetto, username!"
```

Notare che nell'esempio utiliziamo la funzione `>>=` per evitare di avere una monade aggiuntiva.

## Approfondimento

L'autenticazione di base è un metodo semplice per proteggere le risorse di un server, ma non è considerata sicura. Le credenziali vengono trasmesse in chiaro, senza alcuna crittografia, e possono essere intercettate. Inoltre, le credenziali vengono solitamente salvate in una stringa di base64, che può essere facilmente decodificata. 

Per aumentare la sicurezza, è consigliato utilizzare metodi di autenticazione più avanzati come OAuth o OAuth 2.0. Tuttavia, l'autenticazione di base può essere utile per rapidi prototipi o in situazioni in cui la sicurezza non è una grande preoccupazione.

## Vedi anche

- Documentazione ufficiale di `Network.HTTP.Simple`: https://hackage.haskell.org/package/http-client
- Tutorial su come effettuare richieste HTTP in Haskell: https://www.codementor.io/@winteriscoming/efficient-networking-in-haskell-using-http-client-and-conduit-abkl1urrt