---
title:    "Gleam: Lettura degli argomenti della riga di comando"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché 

Se stai iniziando a imparare il linguaggio di programmazione Gleam, una delle prime abilità da acquisire è la lettura degli argomenti della riga di comando. Questa abilità ti consentirà di creare programmi che possano interagire con l'utente e accettare input durante l'esecuzione.

## Come 

Per leggere gli argomenti della riga di comando in Gleam, è necessario utilizzare la funzione `os.args()` che restituisce una lista di stringhe contenente gli argomenti passati al programma durante l'avvio. Ad esempio, se abbiamo il seguente programma `hello_world.gleam`:

```
import gleam/os

fn main() {
    let args = os.args()
    io.println("Ciao" ++ args[0]) // args[0] è il primo argomento della lista
}
```

Quando eseguiamo il programma utilizzando il terminale e forniamo un argomento come `Juan`, l'output sarà `Ciao Juan`. Questo perché `Juan` sarà il primo elemento nella lista restituita dalla funzione `os.args()`.

## Deep Dive 

È importante notare che gli argomenti della riga di comando sono sempre trattati come stringhe in Gleam. Ciò significa che è necessario eseguire il casting esplicito se si vuole utilizzare l'argomento come un numero o un altro tipo di dato.

Inoltre, possiamo anche accedere all'intera lista degli argomenti utilizzando la funzione `os.args_as_list()`. Questo può essere utile se si vuole elaborare più di un argomento o si vuole fare operazioni su tutta la lista.

## See Also 

Ecco alcuni link utili che ti aiuteranno a saperne di più sulla lettura degli argomenti della riga di comando in Gleam:

- Tutorial su come leggere gli argomenti della riga di comando: [Link](https://gleam.run/book/getting-started/command-line-arguments.html)
- Documentazione ufficiale sulle funzioni `os.args()` e `os.args_as_list()`: [Link](https://gleam.run/modules/os.html#fn.args)
- Esempi pratici su come utilizzare gli argomenti della riga di comando in Gleam: [Link](https://github.com/gleam-lang/gleam/blob/master/examples/command-line-args/)

Speriamo che questo breve articolo ti sia stato utile per imparare come leggere gli argomenti della riga di comando in Gleam. Continua a esplorare questo linguaggio di programmazione funzionale per scoprire le sue potenzialità e realizzare progetti sempre più avanzati!