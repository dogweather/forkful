---
title:                "Fish Shell: Maiuscolizzare una stringa"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui uno potrebbe voler capitalizzare una stringa di testo nel proprio codice Fish Shell. Ad esempio, potresti voler dare un aspetto più pulito e facile da leggere ai tuoi output, o potresti essere semplicemente un perfezionista che vuole che tutto sia scritto correttamente.

## Come Fare

Per capitalizzare una stringa nel Fish Shell, puoi utilizzare il comando `string capitalize` seguito dalla stringa che desideri capitalizzare. Ecco un esempio:

```Fish Shell
string capitalize "ciao a tutti"
```
 
Questo produrrà un output di `Ciao a tutti`. Puoi anche specificare più stringhe separate da spazi per capitalizzare simultaneamente più parole.

## Approfondimento

Il comando `string capitalize` è una funzione molto utile e versatile nel Fish Shell. Non solo può capitalizzare l'iniziale di ogni parola in una stringa, ma può anche capitalizzare solo la prima lettera di una singola parola. Ad esempio, se la stringa è `ciao` il risultato sarebbe `Ciao`, mentre se la stringa è `CIAO` il risultato sarebbe ancora `Ciao`. Inoltre, questa funzione funziona correttamente anche con stringhe che includono caratteri speciali e numeri.

## Vedi Anche

- [Documentazione del comando `string capitalize`](https://fishshell.com/docs/current/cmds/string-capitalize.html)
- [Esempi di utilizzo del comando `string capitalize`](https://dev.to/username/comparable-programs-in-bash-and-fish-shell-string-manipulation-3168)