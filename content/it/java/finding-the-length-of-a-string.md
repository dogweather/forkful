---
title:                "Java: Trova la lunghezza di una stringa"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

La lunghezza di una stringa è un concetto fondamentale nella programmazione Java. Conoscere la lunghezza di una stringa può aiutare a manipolarla in modo efficace e a creare algoritmi efficienti. 

## Come fare

Per ottenere la lunghezza di una stringa in Java, si può utilizzare il metodo `length()` della classe `String`. Ecco un esempio di codice:

```Java
// Dichiarazione di una stringa
String parola = "Ciao a tutti!";

// Utilizzo del metodo length()
int lunghezza = parola.length();

// Stampa della lunghezza
System.out.println("La lunghezza della stringa è: " + lunghezza);
```

Output:

```
La lunghezza della stringa è: 12
```

## Approfondimento

Il metodo `length()` ritorna il numero di caratteri presenti nella stringa, incluso lo spazio. Inoltre, è importante notare che il metodo non accetta argomenti, quindi non è possibile specificare una sottostringa per ottenere la sua lunghezza. 

Inoltre, è possibile ottenere la lunghezza di una stringa utilizzando l'operatore "`.`" seguito dalla parola chiave `length`, ad esempio `parola.length()`. Questo è possibile perché `length()` è un metodo membro della classe `String`.

## Vedi anche

- [Java String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String length() method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)