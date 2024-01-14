---
title:                "Bash: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività comune in programmazione, soprattutto quando si lavora con giochi o applicazioni che richiedono una certa imprevedibilità. I numeri casuali sono anche utili per testare algoritmi o eseguire simulazioni.

## Come Fare

Per generare numeri casuali in Bash, possiamo utilizzare il comando `shuf`, che ci permette di mescolare in modo casuale una lista di numeri. Ad esempio, se vogliamo generare un numero casuale compreso tra 1 e 10, possiamo usare il seguente codice:

```Bash
shuf -i 1-10 -n 1
```

Questo comando genera un numero casuale tra 1 e 10 e lo stampa sullo schermo. Possiamo anche generare una lista di numeri casuali utilizzando `shuf`, specificando l'intervallo e il numero di elementi desiderati:

```Bash
shuf -i 1-100 -n 5
```

Questo codice genererà 5 numeri casuali tra 1 e 100 e li stamperà in righe separate.

## Approfondimento

La randomicità è un concetto importante nella generazione di numeri casuali. Molti algoritmi di generazione di numeri casuali si basano su processi fisici o matematici per garantire un risultato più casuale possibile. In Bash, il comando `shuf` utilizza il generatore di numeri casuali di GNU, che si basa sull'algoritmo di Fisher-Yates per garantire una randomicità efficiente.

Tuttavia, è importante notare che l'algoritmo di Fisher-Yates non è completamente casuale e potrebbe essere influenzato da vari fattori esterni. Se si necessita di una randomicità più avanzata, si potrebbe considerare l'utilizzo di un altro strumento o metodo per generare numeri casuali.

## Vedi anche

- [Documentazione di `shuf` su GNU.org](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Algoritmo di Fisher-Yates su Wikipedia](https://it.wikipedia.org/wiki/Algoritmo_di_Fisher-Yates)