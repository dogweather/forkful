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

# Perché inviare una richiesta HTTP in Haskell?

Ci sono molte ragioni per cui potresti voler inviare una richiesta HTTP in Haskell. Potresti aver bisogno di comunicare con un server, accedere a dati da un API o semplicemente testare il tuo codice. In ogni caso, inviare richieste HTTP è fondamentale per molte applicazioni moderne e imparare come farlo in Haskell può essere molto utile.

# Come farlo in Haskell

Per inviare una richiesta HTTP in Haskell, utilizzeremo la libreria "http-conduit". Iniziamo importando il modulo:

```Haskell
import Network.HTTP.Conduit
```

Ora possiamo creare una richiesta GET semplice. Dobbiamo prima definire l'URL della nostra richiesta:

```Haskell
url = "https://www.google.com"
```

Poi possiamo creare la nostra richiesta utilizzando la funzione `parseRequest`. Passiamo il metodo HTTP (in questo caso, "GET") e l'URL:

```Haskell
request = parseRequest "GET" url
```

Ora che abbiamo la nostra richiesta, possiamo inviarla utilizzando `httpLbs`. Questa funzione restituisce una struttura di dati che rappresenta la risposta del server. Possiamo utilizzare la funzione `responseBody` per accedere al contenuto della risposta:

```Haskell
response <- httpLbs request
let body = responseBody response
```

Ecco un esempio completo di come inviare una richiesta HTTP e stampare il suo contenuto:

```Haskell
import Network.HTTP.Conduit

main = do
    let url = "https://www.google.com"
    request <- parseRequest "GET" url
    response <- httpLbs request
    let body = responseBody response

    putStrLn $ "La risposta di " ++ url ++ " è: "
    print body
```

Output:

```
La risposta di https://www.google.com è:
"Il corpo della risposta qui"
```

# Approfondimento

Oltre alle richieste GET, esistono anche altre tipologie di richieste HTTP, come POST, PUT e DELETE. Possiamo utilizzare la funzione `urlEncodedBody` per specificare dei parametri nella nostra richiesta POST, ad esempio:

```Haskell
request <- parseRequest "POST" url
let body = "nome=John&cognome=Doe"
requestWithBody = urlEncodedBody [("nome", "John"), ("cognome", "Doe")] request
```

Inoltre, possiamo impostare degli header personalizzati nella nostra richiesta utilizzando la funzione `setRequestHeader`. Ad esempio, per specificare il tipo di contenuto della nostra richiesta come JSON, possiamo fare così:

```Haskell
request <- parseRequest "POST" url
let body = "nome=John&cognome=Doe"
let headers = [("Content-Type", "application/json")]
requestWithBodyAndHeaders = setRequestHeader headers requestWithBody
```

Ora che abbiamo appreso come inviare richieste HTTP in Haskell, possiamo utilizzarle in varie applicazioni per comunicare con server e accedere a dati. Ricordati sempre di gestire gli errori e di chiudere la connessione HTTP dopo ogni richiesta.

# Vedi anche

- [Documentazione ufficiale di http-conduit](https://hackage.haskell.org/package/http-conduit)
- [Esempi di utilizzo di http-conduit](https://github.com/snoyberg/http-conduit/blob/master/examples/Examples.hs)