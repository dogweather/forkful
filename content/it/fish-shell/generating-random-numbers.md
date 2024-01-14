---
title:    "Fish Shell: Generazione di numeri casuali"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile per la creazione di giochi, la gestione di password o semplicemente per scopi accademici.

## Come fare

Per generare numeri casuali nel Fish Shell, possiamo utilizzare il comando `math random` seguito dal numero massimo di cui vogliamo generare il numero. Per esempio, se vogliamo generare un numero casuale compreso tra 1 e 10, possiamo scrivere:

```Fish Shell
math random 10
```

In questo modo, ogni volta che eseguiremo il comando, otterremo un numero diverso compreso tra 1 e 10. Possiamo anche utilizzare i numeri generati per assegnarli a variabili utilizzando il comando `set` come nel seguente esempio:

```Fish Shell
set numero (math random 100)
echo "Il numero casuale generato è $numero"
```

Questo comando ci restituirà il numero casuale generato e lo assegnerà alla variabile `$numero`.

## Approfondimento

Il comando `math random` utilizza un algoritmo di generazione di numeri pseudo-casuali per generare i numeri casuali. Ciò significa che i numeri generati non sono veramente casuali, ma sono determinati da un algoritmo matematico. Se vogliamo generare numeri veramente casuali, possiamo utilizzare il comando `uuidgen` che utilizza dati del sistema per generare numeri casuali.

Tuttavia, per la maggior parte delle applicazioni, i numeri pseudo-casuali generati dal comando `math random` sono sufficientemente buoni.

## Vedi anche

- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Articolo su come generare numeri casuali in Bash](https://linuxhint.com/generate_random_number_bash/) 
- [Esempi di utilizzo del comando `uuidgen`](https://www.geeksforgeeks.org/uuid-and-uuid_generate-function-in-c-with-examples/)