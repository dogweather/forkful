---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

L'eliminazione dei caratteri corrispondenti a un modello è un'azione che permette di eliminare specifiche sequenze di caratteri in base a un modello o a un'espressione regolare. Questa tecnica è spesso utilizzata per pulire o filtrare i dati di input e per la manipolazione delle stringhe.

## Come si fa:

Gleam non include nativamente funzioni per l'uso di espressioni regolari, ma possiamo utilizzare il modulo Regex di Erlang. Ecco un esempio:

```gleam
import erlang/regex
import gleam/bool.{to_string}

fn main() {
   let re = "^a"
   let string = "abc abc"
   case regex.run(re, string) {
      Ok(_) -> io.println("Match trovato!")
      Error(_) -> io.println("Nessun match!")
   }
}
```

Se esegui questo codice, vedrai stampato `Match trovato!` perché la stringa inizia con `a`.

## Approfondimento

L'eliminazione dei caratteri corrispondenti a un modello nasce con le espressioni regolari, che sono una caratteristica importante dei linguaggi di programmazione storici come Perl e Python. In Gleam, dobbiamo decidere di integrare le funzionalità Erlang per utilizzare le espressioni regolari.

Un'alternativa all'uso di espressioni regolari potrebbe essere l'iterazione attraverso la stringa e l'eliminazione manuale dei caratteri che corrispondono a un criterio specifico, ma questo può essere un processo più lento e intensivo di codice.

La funzione `regex.run` di Erlang in realtà compila l'espressione regolare in un automa a stati finiti, che è quindi utilizzato per la corrispondenza con la stringa. Questo rende il processo di elimina dei caratteri molto efficiente.

## Guarda Anche

Ecco alcuni link utili per approfondire l'argomento:

1. [Documentazione Regex di Erlang](http://erlang.org/doc/man/re.html)
2. [Guida alla sintassi Regex](https://www.regular-expressions.info/tutorial.html)
3. [Documentazione sulla programmazione funzionale in Gleam](https://gleam.run/book/tour/)