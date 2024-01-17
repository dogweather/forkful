---
title:                "Scrivere sull'errore standard"
html_title:           "Gleam: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa e perchè?

Scrivere su standard error è un'attività comune tra i programmatori. Si tratta di inviare messaggi di errore e di debug al terminale invece di stamparli nel normale flusso di output. Questo permette ai programmatori di identificare e risolvere gli errori più facilmente.

## Come fare:

Per scrivere su standard error in Gleam, utilizza la funzione ```error!``` seguita dal messaggio di errore o di debug desiderato all'interno di un blocco di codice ```Gleam```. Ad esempio:

```Gleam
let error_message = "Errore: valore non valido"
error!(error_message)
```

Questo esempio scriverà il messaggio di errore "Errore: valore non valido" su standard error. 

## Approfondimento

Scrivere su standard error è una pratica comune che si è sviluppata negli anni per consentire ai programmatori di identificare facilmente gli errori nel codice. Alcune alternative a questa pratica includono la stampa degli errori sul normale flusso di output o l'utilizzo di un debugger. Tuttavia, scrivere su standard error rimane uno strumento utile e semplice per la gestione degli errori.

In Gleam, la funzione ```error!``` è implementata attraverso il modulo ```std/io``` che, a sua volta, utilizza librerie di basso livello per gestire la comunicazione con standard error.

## Vedi anche

Per ulteriori informazioni su ```error!``` e sulle funzioni di gestione degli errori in Gleam, consulta la documentazione ufficiale del linguaggio: [https://gleam.run/documentation/](https://gleam.run/documentation/).