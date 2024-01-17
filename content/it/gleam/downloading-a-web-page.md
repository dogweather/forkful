---
title:                "Scaricare una pagina web."
html_title:           "Gleam: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scaricare una pagina web è il processo di recuperare il contenuto di una pagina web e salvarlo sul proprio dispositivo. I programmatori spesso eseguono questa operazione per analizzare il contenuto di una pagina, estrarne informazioni o utilizzarlo per creare applicazioni web.

## Come Fare:

In Gleam, è possibile utilizzare il modulo `Http` per scaricare una pagina web. Ecco un esempio di come farlo:

```
Gleam import Http

let response = Http.get("https://esempio.com")

case response {
  Ok(_, body) -> body
  Error(_) -> "Errore nel scaricare la pagina"
}
```

Questo codice utilizza la funzione `get` del modulo `Http` per effettuare una richiesta alla pagina `esempio.com`. Se la richiesta ha successo, verrà restituito il corpo della risposta nella variabile `body`. Se ci sono errori durante il download, verrà stampato un messaggio di errore.

## Approfondimento:

Scaricare pagine web è diventato una parte essenziale della programmazione moderna, poiché ci sono sempre più dati disponibili online. Esistono anche alternative a Gleam per scaricare pagine web, come il modulo `Httpc` di Erlang. L'implementazione di questa funzionalità richiede l'utilizzo dei protocolli HTTP e HTTPS.

## Vedi Anche:

Consulta la documentazione di Gleam sul modulo `Http` per ulteriori informazioni sull'uso di questa funzione. Puoi anche esplorare altri moduli utili per la manipolazione di dati web in Gleam, come `Html` e `Json`.