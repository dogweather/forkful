---
title:                "Bash: Trovare la lunghezza di una stringa"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Lo scopo di questo articolo è quello di spiegare come trovare la lunghezza di una stringa utilizzando il linguaggio di programmazione Bash. La conoscenza di questo concetto è utile per la manipolazione e l'elaborazione delle stringhe all'interno di uno script Bash.

## Come fare

Per ottenere la lunghezza di una stringa in Bash, possiamo utilizzare il parametro integrato `#` seguito dalla variabile della stringa. Ad esempio:

```Bash
stringa="Ciao mondo!"
echo ${#stringa}
```

Questo codice produrrà l'output `12`, poiché la stringa è composta da 12 caratteri (incluso lo spazio). È importante notare che non dobbiamo aggiungere spazi tra il segno `$` e il nome della variabile, altrimenti il comando potrebbe non funzionare correttamente.

Possiamo anche ottenere la lunghezza di una stringa direttamente all'interno di un'istruzione `if`. Ad esempio:

```Bash
stringa="Hello world!"
if [ ${#stringa} -gt 10 ]
then
  echo "La stringa ha più di 10 caratteri."
fi
```

In questo caso, l'output sarà "La stringa ha più di 10 caratteri" poiché la lunghezza della stringa è uguale a 12, quindi maggiore di 10.

Possiamo anche utilizzare il parametro `#` per trovare la lunghezza di un array di stringhe. Ad esempio:

```Bash
nazioni=("Italia" "Francia" "Spagna" "Germania")
echo ${#nazioni[@]}
```

Questo codice produrrà l'output `4`, poiché l'array è composto da 4 elementi.

## Approfondimento

Per comprendere meglio come funziona il parametro `#`, dobbiamo comprendere il significato della variabile `$`. Questo simbolo indica che il testo che segue è una variabile e deve essere sostituito con il suo valore. Nel nostro esempio, il comando `echo` verrà eseguito con la lunghezza della variabile `stringa`, e non con la variabile stessa.

Inoltre, il parametro `#` non è limitato solo alle stringhe. Può essere utilizzato anche per ottenere la lunghezza di un array o di una variabile numerica. Quando viene utilizzato con un array, il parametro restituisce il numero di elementi contenuti nell'array. Quando invece viene utilizzato con una variabile numerica, restituisce il numero di cifre.

## Vedi anche

- [Bash Beginners Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/html_node/)
- [Linuxize - Basic Bash Scripting](https://linuxize.com/post/bash-scripting-tutorial-for-beginners/)