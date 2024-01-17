---
title:                "Trova la lunghezza di una stringa"
html_title:           "Java: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## Cosa e perché?

Trovare la lunghezza di una stringa è un'operazione comune nel mondo della programmazione. Essenzialmente, consiste nel determinare il numero di caratteri all'interno di una determinata stringa. Questo può essere utile per una varietà di motivi, come ad esempio nell'analisi dei dati o nella manipolazione di stringhe.

## Come:

Per trovare la lunghezza di una stringa in Java, è possibile utilizzare il metodo length() della classe String. Ecco un esempio di codice che mostra come farlo:

```java
String str = "Questo è un esempio";
int lunghezza = str.length();
System.out.println(lunghezza);
```

Questo codice stamperà il valore 19, poiché ci sono 19 caratteri nella stringa "Questo è un esempio".

## Approfondimento:

La necessità di trovare la lunghezza di una stringa risale ai primi giorni della programmazione, quando i computer erano limitati dal numero di caratteri che potevano gestire. Oggi, ci sono anche altri modi per ottenere la lunghezza di una stringa, come ad esempio utilizzando il metodo size() della classe Collection.

Tuttavia, il metodo length() rimane il modo più efficiente e compatto per trovare la lunghezza di una stringa in Java. Inoltre, questo metodo è disponibile su molte altre lingue di programmazione, rendendolo una conoscenza fondamentale per tutti i programmatori.

## Vedi anche:

Per ulteriori informazioni sul metodo length() e su altre funzioni disponibili per la manipolazione delle stringhe in Java, puoi consultare la documentazione ufficiale di Java.

https://docs.oracle.com/javase/10/docs/api/java/lang/String.html