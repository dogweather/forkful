---
title:                "Bash: Generazione di numeri casuali"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

La generazione di numeri casuali è un'abilità utile per molti programmatori Bash. Può essere utile per diversi scopi, come la creazione di programmi di gioco o l'inserimento di variabilità nei dati.

## Come Fare

Il modo più semplice per generare un numero casuale in Bash è utilizzando il comando `shuf`. Ad esempio, per generare un numero da 1 a 10, si può utilizzare il seguente codice:

```Bash
shuf -i 1-10 -n 1
```

In questo caso, il comando `shuf` sta generando un numero casuale tra 1 e 10 utilizzando l'opzione `-i` per indicare l'intervallo e l'opzione `-n` per indicare il numero di output desiderato (in questo caso solo uno). L'output potrebbe essere ad esempio 5.

Si può anche utilizzare il comando `random` per generare un numero casuale tra due intervalli. Ad esempio, il seguente codice restituirà un numero casuale tra 10 e 20:

```Bash
echo $((RANDOM%11+10))
```

In questo caso, stiamo utilizzando l'espressione `$((RANDOM%11+10))` per generare un numero casuale tra 0 e 10 (utilizzando il comando `RANDOM`) e poi aggiungendo 10 per ottenere un numero tra 10 e 20.

## Approfondimento

Ci sono diverse tecniche per generare numeri casuali in Bash, come l'utilizzo di funzioni come `srand()` e `rand()`, simili a quelli utilizzati in altri linguaggi di programmazione. Esiste anche la possibilità di generare numeri casuali basati su una lista di seed, utilizzando il comando `shuf` in combinazione con `sed`.

Inoltre, è importante notare che le tecniche di generazione di numeri casuali in Bash possono non essere totalmente sicure per scopi di crittografia o sicurezza. Per questi scopi, è necessario utilizzare specifiche librerie o strumenti.

## Vedi Anche

- [Documentazione ufficiale di `shuf` in Bash](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Articolo su come generare numeri casuali sicuri in Bash](https://www.2daygeek.com/bash-generate-random-number/)
- [Esempi di codice per la generazione di numeri casuali in Bash](https://www.shell-tips.com/bash/random-number/)