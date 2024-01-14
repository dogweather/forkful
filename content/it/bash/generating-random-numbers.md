---
title:    "Bash: Generazione di numeri casuali"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un utile strumento per diversi scopi in programmazione. Ad esempio, potresti voler creare giochi casuali o generare password complesse e sicure per i tuoi account online.

## Come fare

Per generare numeri casuali in Bash, è possibile utilizzare il comando `shuf`. Vediamo un esempio pratico di come utilizzarlo per ottenere un numero casuale compreso tra 1 e 10:

```Bash
echo $(shuf -i 1-10 -n 1)
```

In questo comando, stiamo utilizzando l'opzione `-i` per specificare l'intervallo di numeri e l'opzione `-n` per specificare il numero di numeri da generare. 

Ecco un esempio di output:

```Bash
6
```

Possiamo anche utilizzare la variabile `$RANDOM` per generare numeri casuali. Questa variabile restituisce un numero compreso tra 0 e 32767. Ecco un esempio:

```Bash
echo $RANDOM
```

Output:

```Bash
12973
```

## Approfondimento

Se vuoi generare numeri casuali con un'alta precisione, puoi utilizzare il comando `od`. Questo comando permette di generare numeri casuali a partire da una fonte di entropia, come ad esempio il movimento del mouse o la lettura di file di sistema. Ecco un esempio:

```Bash
echo $(od -An -N2 -i /dev/random)
```

In questo caso, stiamo utilizzando l'opzione `-N` per specificare il numero di byte da leggere, l'opzione `-i` per impostare il formato di output su intero e l'opzione `-An` per rimuovere gli spazi e i caratteri di nuova riga dall'output.

Output:

```Bash
29378
```

Inoltre, è possibile utilizzare la funzione `RANDOM` per generare numeri casuali in una determinata intervallo. Ad esempio, se vogliamo generare un numero casuale tra 1 e 100, possiamo utilizzare il seguente codice:

```Bash
echo $((RANDOM%100+1))
```

Output:

```Bash
54
```

## Vedi anche

- [Shuf manuale] (https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [OD manuale] (https://www.gnu.org/software/coreutils/manual/html_node/od-invocation.html)
- [Guida all'uso di RANDOM] (https://tldp.org/LDP/abs/html/randomvar.html)