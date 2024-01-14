---
title:                "Gleam: Lettura degli argomenti della riga di comando."
simple_title:         "Lettura degli argomenti della riga di comando."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Perché

Hai mai avuto bisogno di leggere gli argomenti dalla riga di comando in un programma scritto in Gleam? Questo articolo ti introdurrà a come farlo in modo facile e veloce.

# Come Fare

Il prossimo esempio illustra come leggere gli argomenti dalla riga di comando in Gleam:

```Gleam
// Questo è il codice del programma
import gleam/io

pub fn main() {
    // Leggi gli argomenti dalla riga di comando
    let args = getopt::args()

    // Stampa gli argomenti uno per uno
    for arg in args {
        io.print(arg)
    }
}
```

Se il nostro programma è chiamato "programma.gleam" e viene eseguito con i seguenti argomenti: "programma.gleam prima seconda terza", l'output mostrerà:

```
prima
seconda
terza
```

# Approfondimento

Oltre a leggere gli argomenti, è anche possibile passare valori come argomenti dalla riga di comando. Ad esempio, se vogliamo passare il nome e l'età come argomenti, il codice sarebbe simile a questo:

```Gleam
// Questo è il codice del programma
import gleam/io
import std/array

type Person(name, age)

pub fn main() {
    // Leggi gli argomenti dalla riga di comando
    let args = getopt::args()

    // Recupera il nome e l'età dall'array args
    let name = array.get(args, 1)
    let age = array.get(args, 2)

    // Crea una nuova persona con i valori passati come argomenti
    let person = Person(name, age)

    // Stampa il nome e l'età della persona
    io.printf("Il nome della persona è %s e ha %s anni.", [person.name, person.age])
}
```

Se il nostro programma è chiamato "programma.gleam" e viene eseguito con i seguenti argomenti: "programma.gleam Marco 30", l'output mostrerà:

```
Il nome della persona è Marco e ha 30 anni.
```

# Vedi Anche

- [Documentazione di getopt](https://gleam.run/libraries/getopt/)
- [Esempi di codice per leggere gli argomenti dalla riga di comando in Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/command-line-arguments.gleam)