---
title:                "Generazione di numeri casuali"
html_title:           "Bash: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile in molti contesti di programmazione, dal testare l'applicazione a creare giochi o algoritmi probabilistici. In questa guida esploreremo come utilizzare il linguaggio di scripting Bash per generare numeri casuali.

## Come fare

Per generare un numero casuale in Bash, possiamo utilizzare il comando *$RANDOM*. Questo comando restituisce un numero intero casuale compreso tra 0 e 32767. Possiamo quindi utilizzare questo numero all'interno di una formula matematica per ottenere un numero in un range specifico.

Ad esempio, per ottenere un numero casuale tra 1 e 100, possiamo utilizzare il seguente comando:

```Bash
echo $((RANDOM % 100 + 1))
```

In questo modo, il comando restituirà un numero casuale tra 1 e 100 ogni volta che viene eseguito.

Possiamo anche utilizzare il comando *shuf* per generare una lista di numeri casuali. Il seguente esempio creerà una lista di 10 numeri casuali compresi tra 1 e 50:

```Bash
shuf -i 1-50 -n 10
```

Questo comando utilizza l'opzione *-i* per specificare il range di numeri, mentre l'opzione *-n* indica il numero di numeri da generare.

## Deep Dive

Il comando *$RANDOM* utilizza un generatore di numeri pseudo-casuali, il che significa che i numeri generati non sono veramente casuali ma sono invece determinati da un algoritmo. Ciò implica che, se il comando *$RANDOM* viene eseguito più volte, i numeri generati saranno sempre gli stessi. Per ottenere numeri realmente casuali, possiamo utilizzare un generatore di numeri esterno come /dev/random o /dev/urandom.

Il file /dev/random restituisce numeri casuali basati sull'entropia del sistema, ma può essere influenzato da fattori esterni come il movimento del mouse o la lettura di dati da dispositivi hardware. D'altra parte, /dev/urandom utilizza un generatore di numeri pseudo-casuali, ma basato su un pool di entropia iniziale. Questo pool viene continuamente aggiornato dalle interazioni del sistema, garantendo numeri casuali di alta qualità senza dover aspettare che il pool si ricarichi come accade con /dev/random.

## Vedi anche

- [Manuale di Bash su $RANDOM](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
- [Tutorial su shuf](https://www.tecmint.com/shuf-command-examples/) 
- [Articolo su /dev/random e /dev/urandom](https://security.stackexchange.com/questions/89/a