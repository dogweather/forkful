---
title:                "Leggere gli argomenti della riga di comando."
html_title:           "Gleam: Leggere gli argomenti della riga di comando."
simple_title:         "Leggere gli argomenti della riga di comando."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Cosa sono e perché i programmatori li utilizzano?
I parametri della riga di comando sono informazioni fornite dal sistema operativo all'avvio di un programma. Sono utilizzati dai programmatori per consentire all'utente di inserire dati aggiuntivi durante l'esecuzione del programma.

Come fare:
Per leggere i parametri della riga di comando in Gleam, è possibile utilizzare la funzione `CommandLine.arguments()`. Di seguito è riportato un esempio di codice:

```Gleam
import gleam/io

fn main(_) {
 let arguments = CommandLine.arguments()
 io.print("I parametri inseriti sono: {arguments}")
}
```

Ecco un esempio di output:

```
> gleam run hello_world.gleam --name "Marco"
I parametri inseriti sono: ["hello_world.gleam","--name","Marco"]
```

Approfondimento:
La lettura dei parametri della riga di comando è una funzionalità comune nei linguaggi di programmazione, poiché consente all'utente di interagire con il programma in modo dinamico. In alternativa a Gleam, è possibile utilizzare la funzione `System.args()` in Erlang, su cui si basa il compilatore di Gleam.

Vedi anche:
- Documentazione di Gleam sulla funzione `CommandLine.arguments()`: [link](https://gleam.run/book/introduction#hello-world)
- Documentazione di Erlang sulla funzione `System.args()`: [link](http://erlang.org/doc/man/sys.html#args-0)
- Esempi di codice su come utilizzare i parametri della riga di comando in Gleam: [link](https://github.com/gleam-lang/gleam_discuss/issues/121)