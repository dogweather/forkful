---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con Gleam

## Che Cos'è e Perché?

L'invio di una richiesta HTTP è un metodo per comunicare con server e servizi web. I programmatori lo usano per interagire con API, recuperare dati, inviare informazioni e molto altro ancora.

## Come si fa:
Ecco un esempio di come si invia una richiesta HTTP GET utilizzando Gleam:

```Gleam 
import gleam/httpc
import gleam/uri.{Uri}

pub fn main(args: List(String)) {
  let uri = uri.parse("http://my-website.com").unwrap()
  let response = httpc.get(uri)
  case response {
    Ok(response) -> io.println(response.status)
    Error(err) -> io.println(err)
  }
}
```
Quando esegui questo codice, vedrai qualcosa del genere:

```Gleam
200 OK
```

## Approfondimento:

### Contesto storico:
L'HTTP (Hypertext Transfer Protocol) è stato sviluppato negli anni '90 come componente fondamentale del World Wide Web. Da allora, è diventato lo standard per la comunicazione tra client e server.

### Alternative:
Oltre a HTTP, esistono altri protocolli come HTTPS (versione sicura di HTTP), FTP, SMTP e altri. Tuttavia, HTTP rimane il più popolare per le applicazioni web moderne.

### Dettagli di implementazione:
Gleam utilizza il modulo `httpc` per inviare richieste HTTP. Un URI viene passato alla funzione `get` per eseguire la richiesta. Il risultato restituito è un `Result` che può essere un `Ok` con la risposta HTTP o un `Error` con un errore HTTP.

## Vedi Anche:

3. Documentazione [HTTP](https://developer.mozilla.org/it/docs/Web/HTTP) da Mozilla Developer Network.