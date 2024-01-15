---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Se stai scrivendo un programma in Gleam, potresti trovarmi nella situazione in cui devi verificare se una directory esiste o meno. Potrebbe essere necessario per accedere a dei file specifici o per gestire diversi percorsi di lavoro.

## Come fare
Per verificare se una directory esiste in Gleam, possiamo utilizzare la funzione `os.dir_exists` del modulo standard `gleam/os`. Passando il percorso della directory come argomento, la funzione restituirà un valore booleano, `true` se la directory esiste e `false` altrimenti. Ecco un esempio di codice che utilizza questa funzione:

```Gleam
import gleam/os

let my_directory = "/path/to/directory"

if os.dir_exists(my_directory) {
  // Directory exists, do something
} else {
  // Directory does not exist, handle error
}
```

Nell'esempio sopra, la variabile `my_directory` contiene il percorso della directory che vogliamo verificare. Utilizzando l'istruzione `if`, possiamo gestire due casi: se la directory esiste eseguiamo una determinata azione, altrimenti gestiamo l'errore o la situazione in cui la directory non esiste.

## Approfondimento
È importante notare che la funzione `os.dir_exists` verifica solo se la directory esiste e non se è un file. Inoltre, è possibile utilizzare il risultato della funzione direttamente all'interno di un'espressione `if` senza dover assegnare risultato ad una variabile. Ad esempio:

```Gleam
import gleam/os

let my_directory = "/path/to/directory"

if os.dir_exists(my_directory) {
  // Utilizza la directory direttamente
} else {
  // Gestisci l'errore
}
```

## Vedi anche
- [Documentazione ufficiale di Gleam sul modulo `os`](https://gleam.run/documentation/standard_library#glos)
- [Un tutorial su come utilizzare `os.dir_exists` in un progetto Gleam](https://gleam.run/getting-started/guides/filesystems)