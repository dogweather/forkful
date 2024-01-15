---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Gleam: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto bisogno di leggere argomenti della riga di comando nel tuo programma? Se sì, allora sei nel posto giusto! In questo articolo, ti mostreremo come leggere facilmente i tuoi argomenti della riga di comando utilizzando Gleam. Continua a leggere per scoprire come!

## Come fare

Per iniziare, dovrai assicurarti di aver installato Gleam sul tuo sistema. Una volta fatto ciò, puoi iniziare a scrivere il tuo codice.

Per leggere gli argomenti dalla riga di comando, prima devi importare il modulo "gleam/cli". Quindi, puoi utilizzare la funzione "read_args" per leggere gli argomenti in un elenco. Ecco un esempio di codice:

```Gleam
import gleam/cli

pub fn main() {
  args = cli.read_args()
  io.println(args)
}
```

Supponiamo che tu abbia chiamato questo file "read_args.gleam" e lo stai eseguendo dalla riga di comando con questi argomenti: "foo bar baz". L'output sarà un elenco contenente i tuoi argomenti, così:

```["foo", "bar", "baz"]```

Se vuoi leggere solo un determinato numero di argomenti, puoi utilizzare la funzione "sublist". Ad esempio, se vuoi solo il secondo e il terzo argomento della lista, puoi fare così:

```Gleam
args = cli.read_args()
subset = sublist(args, 1, 3)
io.println(subset)
```

In questo caso, l'output sarà:

```["bar", "baz"]```

## Approfondimenti

Adesso che hai visto come leggere gli argomenti della riga di comando, vediamo alcune altre funzioni utili del modulo "gleam/cli".

Per prima cosa, puoi utilizzare la funzione "get_arg" per ottenere un argomento specifico dalla lista degli argomenti. Ad esempio, se vuoi solo il secondo argomento, puoi fare così:

```Gleam
args = cli.read_args()
second_arg = cli.get_arg(1, args)
io.println(second_arg)
```

In questo caso, l'output sarà:

```"bar"```

Inoltre, puoi controllare se un argomento specifico è stato passato utilizzando la funzione "has_arg". Ad esempio, se vuoi controllare se il terzo argomento è stato passato, puoi fare così:

```Gleam
args = cli.read_args()
has_third_arg = cli.has_arg(2, args)
io.println(has_third_arg)
```

L'output in questo caso sarà:

```True```

Ecco! Ora sei pronto per leggere gli argomenti della riga di comando come un professionista utilizzando Gleam.

## Vedi anche

- [Documentazione di Gleam CLI](https://gleam.run/documentation/)
- [Tutorial di Gleam](https://gleam.run/getting-started/)
- [Altri articoli su Gleam per imparare di più](https://gleam.run/articles/)