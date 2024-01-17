---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Cosa e Perché?

Trovare la lunghezza di una stringa è una delle operazioni più comuni che i programmatori eseguono quando lavorano con stringhe. La lunghezza di una stringa si riferisce semplicemente al numero di caratteri presenti all'interno della stringa stessa. Questa operazione viene eseguita per diverse ragioni, come ad esempio per l'elaborazione dei dati, il controllo della validità di una stringa o per la formattazione.

# Come Fare:

Per trovare la lunghezza di una stringa in Haskell, possiamo utilizzare la funzione "length". Vediamo un esempio:

```Haskell
length "Ciao Mondo"   -- output: 11
```

In questo esempio, la funzione "length" prende come argomento la stringa "Ciao Mondo" e restituisce il risultato 11, poiché questa è la lunghezza della stringa.

Possiamo anche utilizzare la funzione "length" su una lista di stringhe, ad esempio:

```Haskell
length ["Ciao", "Mondo"]   -- output: 2
```

In questo caso, la funzione restituirà la lunghezza della lista, che in questo caso è 2.

# Approfondimenti:

La funzione "length" è stata introdotta per la prima volta nel linguaggio di programmazione LISP nel 1959 ed è stata successivamente adottata da molti altri linguaggi, tra cui Haskell. Tuttavia, alcuni linguaggi come Python forniscono un metodo integrato per ottenere la lunghezza di una stringa, come ad esempio "len". Inoltre, è possibile calcolare la lunghezza di una stringa anche utilizzando un ciclo o una ricorsione, ma questa è una soluzione meno efficiente rispetto all'utilizzo della funzione "length".

# Vedere Anche:

Per ulteriori informazioni sulla funzione "length" e su come gestire le stringhe in Haskell, si può consultare la documentazione ufficiale del linguaggio o i numerosi tutorial e guide disponibili online. È anche possibile esplorare altre funzioni utili per l'elaborazione delle stringhe in Haskell, come ad esempio "concat", "take" e "drop".