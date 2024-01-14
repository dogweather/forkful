---
title:                "Gleam: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

L'invio di una richiesta HTTP è fondamentale per interagire con il web. Se stai sviluppando un sito web, un'applicazione o un servizio web, dovrai comprendere come inviare una richiesta HTTP per ottenere o inviare dati.

## Come fare

Gleam offre un modo semplice e intuitivo per inviare richieste HTTP utilizzando il modulo `http` nella sua libreria standard. Vediamo un esempio pratico di come fare:

```
Gleam - 
let url = "https://example.com/api"
let body = Json.Encode.object([("name", Json.Encode.string("John")), ("age", Json.Encode.int(30))])
let response = http.post(url, Http.request(body))
```

Il codice sopra invierà una richiesta HTTP `POST` all'URL specificato con un corpo contenente un oggetto JSON con una chiave "name" e "age". Il tipo di `response` sarà `Result(http.Error, http.Response)`, quindi possiamo gestire la risposta utilizzando il costrutto `case`.

```
Gleam - 
let result = case response {
  Ok(response) -> Json.Decode.map(fetchUser, response.body)
  Err(_) -> Json.Decode.fail("Errore durante l'invio della richiesta")
}
```

In questo esempio, stiamo gestendo la risposta HTTP correttamente e decodificando i dati ricevuti in un oggetto con la funzione `fetchUser`. Tuttavia, se ci fosse un errore durante la richiesta, saremmo in grado di gestirlo in modo da fornire un adeguato feedback all'utente.

## Approfondimenti

Mentre l'esempio sopra copre i fondamenti dell'invio di una richiesta HTTP con Gleam, ci sono molte altre funzionalità e opzioni disponibili per personalizzare le tue richieste. Puoi sfruttare gli header, passare parametri o utilizzare metodi diversi per la richiesta. Per saperne di più, puoi consultare la documentazione ufficiale di Gleam sul modulo `http`.

## Vedi anche

- Documentazione ufficiale di Gleam: https://gleam.run/documentation
- Esempi di utilizzo di `http` in Gleam: https://github.com/gleam-lang/gleam_exa