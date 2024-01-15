---
title:                "Scrivere su errore standard"
html_title:           "Gleam: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché Scrivere a Standard Error in Gleam?

Scopriamo insieme perché scrivere a standard error in Gleam può semplificare la vita del programmatore e migliorare la qualità del codice.

## Come Farlo

Per scrivere a standard error in Gleam, si utilizza la funzione `gleam/io#stderr` seguita dal metodo `write` e tra parentesi tonde il messaggio che si desidera scrivere.

```
Gleam.mod
fn main() {
  gleam/io.stderr
  |> write("Ciao, questo è un messaggio di errore!")
}
```

Questo codice scriverà a standard error il messaggio "Ciao, questo è un messaggio di errore!" ogni volta che il programma viene eseguito.

```
$ gleam run main.gleam
> Ciao, questo è un messaggio di errore!
```

## Approfondimento

Scrivere a standard error può essere utile nel gestire gli errori nei nostri programmi. Invece di scrivere messaggi di errore lungo il codice, possiamo utilizzare questa funzione per centralizzare le informazioni e renderle più facili da gestire. Inoltre, scrivere a standard error ci permette di distinguere i messaggi di errore dai messaggi di output, migliorando la leggibilità del codice.

## Vedi Anche

- [Documentazione Gleam sull'utilizzo di standard error](https://gleam.run/book/tutorials-and-examples/standard-error.html)
- [Esempi di codice su GitHub utilizzando standard error in Gleam](https://github.com/search?q=gleam+stderr)