---
title:                "Fish Shell: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività comune in molti linguaggi di programmazione, inclusa la shell di Fish. L'uso di numeri casuali può essere utile in diversi scenari, come creare dati casuali per scopi di testing o simulare situazioni casuali in un programma.

## Come fare

Per generare numeri casuali nella shell di Fish, è possibile utilizzare il comando `random` seguito da un intervallo di numeri tra parentesi tonde. Ad esempio, per generare un numero casuale compreso tra 1 e 10, è possibile utilizzare il seguente comando:

```Fish Shell
random(1 10)
```

Il comando restituirà un numero casuale all'interno di quell'intervallo. Possiamo anche specificare un numero specifico di numeri casuali da generare aggiungendo un argomento al comando. Ad esempio, il seguente comando genererà 5 numeri casuali compresi tra 1 e 10:

```Fish Shell
random 5 (1 10)
```

È anche possibile specificare un intervallo di numeri decimali invece che interi, aggiungendo semplicemente la parola `float` dopo il comando `random`. Ad esempio:

```Fish Shell
random float (1 10)
```

Il comando restituirà un numero casuale con la virgola all'interno dell'intervallo specificato.

## Approfondimento

La generazione di numeri casuali nella shell di Fish utilizza l'algoritmo di generazione di numeri casuali di Mersenne Twister. Questo algoritmo è ampiamente utilizzato per la sua alta qualità e periodicità estremamente lunga.

Inoltre, è possibile generare numeri casuali utilizzando svariati intervalli e distribuzioni, come la distribuzione gaussiana o la distribuzione di Poisson. Per fare ciò, è possibile utilizzare la libreria `random` della shell di Fish che fornisce una gamma di funzioni per la generazione di numeri casuali in diversi formati.

## Vedi anche

- Documentazione ufficiale dei comandi di `random` in Fish Shell: https://fishshell.com/docs/current/cmds/random.html
- Esempi di codice per la generazione di numeri casuali in Fish Shell: https://github.com/fish-shell/fish-shell/issues/3211
- Una guida completa alla generazione di numeri casuali in Fish Shell: https://www.systutorials.com/241027/generate-random-numbers-fish-shell/