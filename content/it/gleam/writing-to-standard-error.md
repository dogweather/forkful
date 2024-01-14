---
title:    "Gleam: Scrivere su standard error"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere al flusso di errore standard (standard error) è una tecnica di debugging molto utile quando si sviluppa in Gleam. Questo è perché i messaggi di errore sono spesso più informativi e dettagliati rispetto ai meri messaggi di output. Inoltre, è possibile gestire questo tipo di output in modo diverso rispetto all'output standard, rendendo più facile identificare e risolvere eventuali problemi nel codice.

## Come fare

Per scrivere al flusso di errore standard in Gleam, è sufficiente utilizzare la funzione `stderr.write/1` e passare come argomento la stringa o il valore che si desidera visualizzare. Ecco un esempio di codice:

```Gleam
let message = "Questo è un messaggio di errore"
stderr.write(message)
```
L'output di questo codice sarà "Questo è un messaggio di errore" stampato al flusso di errore standard. Oltre alle stringhe, è possibile passare anche altri tipi di dati come argomento alla funzione, come ad esempio record, tuple o liste.

## Approfondimento

In Gleam, il flusso di errore standard è gestito dal modulo `stderr`, che offre una varietà di funzioni per scrivere e formattare l'output. È possibile utilizzare la funzione `stderr.format/2` per formattare l'output utilizzando il sintassi di interpolazione delle stringhe, o la funzione `stderr.write_line/1` per scrivere automaticamente una nuova riga dopo l'output. Inoltre, è possibile utilizzare la funzione `stderr.write_bytes/1` per scrivere dati binari direttamente al flusso di errore standard.

## Vedi anche

- [Documentazione di Gleam su stderr](https://gleam.run/book/stdlib.html#stderr)
- [Esempi di utilizzo di stderr in Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/log/Log.gleam)