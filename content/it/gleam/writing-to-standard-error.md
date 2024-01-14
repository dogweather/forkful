---
title:    "Gleam: Scrivere su standard error"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché scrivere a standard error in Gleam

Se stai sviluppando un'applicazione in Gleam, potresti chiederti perché dovresti scrivere a standard error. La risposta è semplice: scrivere a standard error può aiutarti a identificare facilmente gli errori e a risolverli più rapidamente durante il processo di debug.

## Come scrivere a standard error in Gleam

Per scrivere a standard error in Gleam, è sufficiente utilizzare la funzione `io.fprintf` e specificare `stderr` come primo argomento. Ad esempio:

```Gleam
let error_message = "Qualcosa è andato storto"
io.fprintf(stderr, "Errore: %@", [error_message])
```

Questo codice scriverà l'errore nella console e sarà visibile in rosso per evidenziare che si tratta di un errore.

## Approfondimento su come e quando scrivere a standard error

Scrivere a standard error è particolarmente utile quando si desidera tracciare gli errori durante il debug di un'applicazione o quando si vogliono fornire informazioni dettagliate agli utenti su cosa è andato storto. In Gleam, gli errori vengono sollevati quando si verifica un errore di tipo `Error` che può essere catturato con il costrutto `try ... catch`.

## Vedi anche

Per ulteriori informazioni su come gestire gli errori in Gleam, puoi consultare la documentazione ufficiale:
- [Gestione degli errori in Gleam](https://gleam.run/book/tour/error-handling.html)
- [Funzione `io.fprintf` nella libreria standard di Gleam](https://gleam.run/modules/gleam_io#fwrite)
- [Esempi di gestione degli errori in Gleam](https://github.com/gleam-lang/gleam/blob/main/examples/error_handling.gleam)