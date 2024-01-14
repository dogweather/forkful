---
title:                "Bash: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un'operazione fondamentale nella programmazione, che può essere utilizzata per molteplici scopi come la validazione dei dati, la correzione di errori o la manipolazione di stringhe.

## Come Fare

Per trovare la lunghezza di una stringa in Bash, è necessario utilizzare il comando `expr length` seguito dalla stringa da controllare tra parentesi. Ad esempio:

```Bash
stringa="Ciao, mondo!"
lunghezza=$(expr length $stringa)
echo $lunghezza #output: 12
```

In questo esempio, abbiamo definito una variabile `stringa` con il valore "Ciao, mondo!" e utilizzando il comando `expr length` abbiamo assegnato il risultato alla variabile `lunghezza`. Infine, con il comando `echo`, abbiamo visualizzato il valore della variabile `lunghezza`, ottenendo come output la lunghezza totale della stringa, ovvero 12 caratteri.

## Approfondimento

Per comprendere meglio come funziona il comando `expr length`, è importante capire che in Bash ogni carattere di una stringa viene considerato come un elemento separato. Ad esempio, nella stringa "Ciao, mondo!" ci sono 12 elementi: la lettera "C", la lettera "a", la lettera "i", la lettera "o", la virgola, lo spazio, la lettera "m", la lettera "o", la lettera "n", la lettera "d", la lettera "o" e il punto esclamativo.

Il comando `expr length` conta il numero di elementi della stringa e restituisce il valore numerico corrispondente. Va notato però che questo comando non considera gli spazi vuoti all'interno della stringa, quindi se volessimo contare anche gli spazi, dovremmo utilizzare il comando `wc` seguito dall'opzione `-c` (caratteri). Ad esempio:

```Bash
stringa="Ciao, mondo!"
lunghezza=$(echo $stringa | wc -c)
echo $lunghezza #output: 13
```

In questo caso, abbiamo utilizzato il comando `echo` per inviare la stringa all'interno della pipe `|` e successivamente abbiamo utilizzato il comando `wc` per contare il numero di caratteri della stringa, ottenendo come risultato 13 (compreso lo spazio vuoto).

## Vedi Anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Tutorial su Bash](https://linuxconfig.org/bash-scripting-tutorial)
- [Guida alla programmazione in Bash](https://www.tecmint.com/bash-variables-constant-arrays-operators-expression-examples/)