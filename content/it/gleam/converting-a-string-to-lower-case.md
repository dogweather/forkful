---
title:    "Gleam: Convertire una stringa in minuscolo"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere utile in tante situazioni, ad esempio per uniformare il testo di input o per facilitare la ricerca di una parola all'interno di un'altra stringa.

## Come fare

Per convertire una stringa in minuscolo in Gleam, possiamo utilizzare la funzione built-in `String.to_lower`, come mostrato nell'esempio seguente:

```Gleam
let stringa = "CIAO MONDO"
let stringa_minuscola = String.to_lower(stringa)
```

L'output di `stringa_minuscola` sarà "ciao mondo".

## Approfondimenti

Ma come funziona esattamente la funzione `String.to_lower`? In realtà, questa funzione non fa altro che applicare il metodo `to_lower` della libreria standard di Erlang alla stringa in input. Questo metodo, a sua volta, si basa sulle regole della Unicode Standard per il case mapping.

Per gli appassionati di linguaggi di programmazione, è interessante notare che in Gleam, come in Erlang, le stringhe sono rappresentate internamente come liste di interi Unicode, anziché come matrici di byte come in molti altri linguaggi. Questa scelta permette una maggiore flessibilità per la gestione delle stringhe in diverse lingue.

## Vedi anche

- [Documentazione ufficiale di Gleam sulle stringhe](https://gleam.run/packages/std/string.html)
- [Documentazione ufficiale di Erlang sulle funzioni di conversione delle stringhe](http://erlang.org/doc/apps/stdlib/unicode_usage.html#to_lower-1)