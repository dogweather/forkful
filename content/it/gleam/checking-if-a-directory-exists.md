---
title:                "Verifica se una directory esiste"
html_title:           "Gleam: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa è e perché?
Verificare se una directory esiste è un'operazione comune nei linguaggi di programmazione, che consente di verificare la presenza di una cartella all'interno del sistema operativo. I programmatori lo fanno per eseguire diverse operazioni, ad esempio, per determinare se devono creare o eliminare una directory, o semplicemente per controllare la correttezza di un percorso fornito dall'utente.

## Come fare:
Controllare l'esistenza di una directory è molto semplice in Gleam, grazie alla sua libreria standard. Basta utilizzare la funzione `exists`, passando come argomento il percorso della directory da verificare. Ecco un esempio di codice:

```Gleam
let directory = "/path/to/directory"
let result = exists(directory)
```

Se la directory esiste, il valore di `result` sarà `true`; in caso contrario, sarà `false`. Possiamo anche utilizzare `match` per gestire entrambi i casi: 

```Gleam
let directory = "/path/to/directory"
match exists(directory) {
  True -> "La directory esiste"
  False -> "La directory non esiste"
}
```

## Approfondimenti:
La necessità di verificare l'esistenza di una directory risale ai primi sistemi operativi, in cui era necessario utilizzare comandi specifici per creare o eliminare cartelle. Oggi, ci sono diversi modi per verificare l'esistenza di una directory in diversi linguaggi di programmazione, come ad esempio utilizzando le API del sistema operativo o le librerie specifiche del linguaggio. Tuttavia, la funzione `exists` in Gleam semplifica notevolmente questo processo.

## Vedi anche:
Per ulteriori informazioni su come utilizzare la funzione `exists` in Gleam, ti consigliamo di consultare la documentazione ufficiale [qui](https://gleam.run/modules/gleam/os.html#exists) e il codice sorgente della libreria standard [qui](https://github.com/gleam-lang/gleam_stdlib/blob/master/gleam/os/exists.gleam).