---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Trovare la Lunghezza di una Stringa in Java
## Cosa & Perché?
Trovare la lunghezza di una stringa è l'atto di contare quanti caratteri ci sono in una stringa. Lo facciamo per sapere quanto "grande" è una stringa in termini di caratteri.

## Come si fa:
Esempio di codice:

```Java
public class LunghezzaStringa {
    public static void main(String[] args) {
        String str = "Ciao a tutti!";
        int lunghezza = str.length();
        System.out.println("La lunghezza della stringa è: " + lunghezza);
    }
}
```
Output di esempio:

```Java
La lunghezza della stringa è: 14
```

## Approfondimenti:
Trovare la lunghezza di una stringa è un'operazione molto vecchia in programmazione; è un'operazione fondamentale cominciata con i primi linguaggi. In Java, il metodo `.length()` per l'analisi della lunghezza di una stringa è stato introdotto sin da Java 1.0.

Esistono molti modi per trovare la lunghezza di una stringa in Java. Uno di questi modi è usare il metodo `.length()`, mentre un altro è convertire la stringa in un array di caratteri e contare gli elementi dell'array.

Il modo in cui il metodo `length()` è implementato in Java è molto efficiente. Infatti, non conta effettivamente i caratteri ogni volta che chiami il metodo; invece, ritorna la lunghezza dell'array di caratteri interno usato per memorizzare la stringa, il che equivale al numero di caratteri della stringa.

## Vedi Anche:
[1] Documentazione Oracle su String: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html

[2] Tutorial Oracle sulla classe String: https://docs.oracle.com/javase/tutorial/java/data/strings.html