---
title:                "Ottenere la data corrente."
html_title:           "Gleam: Ottenere la data corrente."
simple_title:         "Ottenere la data corrente."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Ottenere la data corrente è una funzione essenziale per i programmatori. Si tratta semplicemente di recuperare la data corrente dal sistema operativo, che può essere utilizzata per una varietà di scopi come registrazione di eventi, tempistica delle attività o sincronizzazione con altri sistemi.

## How to:

Per ottenere la data corrente in Gleam, è sufficiente utilizzare la funzione "date.now()" seguita dal formato desiderato. Ad esempio:

```Gleam
let current_date = date.now() |> date.format("%m/%d/%Y") 
```

Il risultato sarà una stringa contenente la data corrente formattata nel formato specificato.

Ecco un esempio di output: 

```
06/26/2021
```

## Deep Dive

L'implementazione di come ottenere la data corrente varia a seconda del sistema operativo. Tuttavia, la maggior parte dei linguaggi di programmazione, tra cui Gleam, fornisce una libreria standard per semplificare questa operazione. 

In alternativa, è possibile utilizzare una libreria di terze parti per ottenere date più precise o con formati personalizzati.

Per gli sviluppatori più esperti, è possibile implementare manualmente l'acquisizione della data utilizzando la libreria FFI (Foreign Function Interface) per comunicare con il sistema operativo.

## See Also

- [Documentazione ufficiale di Gleam per la funzione date.now()](https://gleam.run/stdlib/date.html#now)
- [Esempi di formati di data supportati da Gleam](https://gleam.run/stdlib/date.html#format_flags)
- [Libreria di terze parti per ottenere date e orari più precisi](https://github.com/gleam-lang/calendar)
- [Documentazione di FFI per Gleam](https://gleam.run/ffi/)