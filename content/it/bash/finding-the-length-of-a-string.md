---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Bash: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui è necessario conoscere la lunghezza di una stringa, ad esempio per la formattazione dei dati o per manipolarla in un modo specifico. Scrivere una semplice riga di codice bash che restituisce la lunghezza della stringa può risparmiare tempo e fatica durante lo sviluppo del progetto.

## Come fare

Per trovare la lunghezza di una stringa in bash, è possibile utilizzare il comando `expr length <stringa>`. Ecco un esempio pratico:

```Bash
stringa="Ciao Mondo!"
echo "La lunghezza della stringa è: " `expr length $stringa`
```

Output:

`La lunghezza della stringa è: 12`

In questo esempio, abbiamo assegnato una stringa alla variabile `stringa` e poi utilizzato il comando `expr length` per trovare la lunghezza della stringa. Nota che abbiamo usato il simbolo `$` prima del nome della variabile quando abbiamo passato la variabile al comando, in questo modo il comando sa che deve utilizzare il valore della variabile anziché il suo nome.

## Approfondimento

Il comando `expr` è uno strumento utile in bash, in quanto consente di eseguire operazioni matematiche e manipolare le stringhe. Per trovare la lunghezza di una stringa, il comando `expr length` conta semplicemente il numero di caratteri nella stringa fornita. Tieni conto che il comando non include gli spazi quando calcola la lunghezza della stringa, quindi assicurati di tenerne conto quando utilizzi questo comando.

Un'alternativa per trovare la lunghezza di una stringa è utilizzare il parametro `#` prima del nome della variabile. Ad esempio, il seguente codice restituisce la stessa lunghezza della stringa del nostro esempio precedente:

```Bash
stringa="Ciao Mondo!"
echo "La lunghezza della stringa è: " ${#stringa}
```

Output:

`La lunghezza della stringa è: 12`

Questo parametro è spesso più semplice da utilizzare rispetto al comando `expr`, ma è utile conoscere entrambi i metodi.

## Vedi anche

- [Comando `expr` su Linuxize](https://linuxize.com/post/expr-command-in-linux/)
- [Parametri nei comandi bash su TecMint](https://www.tecmint.com/use-linux-shell-variables-in-expr-commands/)