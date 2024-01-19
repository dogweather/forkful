---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perchè?

La lettura degli argomenti della riga di comando permette di passare informazioni a un programma durante la sua esecuzione. I programmatori lo fanno per rendere i loro programmi più flessibili e personalizzabili.

## Come fare:

In Gleam, si potrebbe usare il modulo `gleam/option` per lavorare con gli argomenti della riga di comando. Ecco un esempio di utilizzo:

```Gleam
import gleam/option.{Some, None}

fn main(args: List(String)) -> Nil {
  case args {
    | [] -> io.println("Nessun argomento passato")
    | [first | rest] -> io.println(first)
  }
  Ok(Nil)
}
```

Allorquando eseguito con un argomento, lo programmo produrrà:
```shell
$ gleam run programma ciao
ciao
```

## Approfondimento:

Storicamente, gli argomenti della riga di comando sono stati utilizzati dagli albori dell'informatica per controllare l'esecuzione dei programmi. Nel contesto di Gleam e linguaggi di programmazione funzionale, la manipolazione degli argomenti della riga di comando viene resa facile dall'utilizzo di liste e pattern matching.

Le alternative alla lettura degli argomenti della riga di comando includono l'input dell'utente in tempo reale, il caricamento di file di configurazione o l'interrogazione di un database.

I dettagli implementativi della lettura degli argomenti della riga di comando in Gleam coinvolgono la funzione `main` che prende una `List(String)` come input. Questo è coerente con il concetto di Erlang che "tutto è un processo", dato che `main` è il punto di ingresso per un nuovo processo.

## Vedi anche:

Per ulteriori informazioni su glem/option, consulta la documentazione ufficiale [qui](https://gleam.run/documentation/library/gleam/option/).

Per maggiori dettagli sulla linea di comando in Erlang, dai un'occhiata a [questo post](https://erlangcentral.org/wiki/index.php/Command_line_parsing).