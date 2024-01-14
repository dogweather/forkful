---
title:                "Java: La ricerca della lunghezza di una stringa"
simple_title:         "La ricerca della lunghezza di una stringa"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si programma in Java, si ha la necessità di conoscere la lunghezza di una stringa. Questo può essere utile per validare gli input dell'utente, manipolare le stringhe o semplicemente per motivi di formattazione. In questo post vedremo come trovare la lunghezza di una stringa in Java.

## Come fare

Per trovare la lunghezza di una stringa in Java, è possibile utilizzare il metodo `length()` della classe `String`. Vediamo un esempio pratico:

```Java
String stringa = "Ciao, mondo!";
System.out.println(stringa.length());
```

Questo codice produrrà in output il numero `13`, che rappresenta la lunghezza della stringa "Ciao, mondo!". Possiamo anche utilizzare questo metodo all'interno di un'operazione di assegnazione, come in questo esempio:

```Java
String saluto = "Ciao";
int lunghezza = saluto.length();
System.out.println("La stringa \"" + saluto + "\" ha una lunghezza di " + lunghezza + " caratteri.");
```

Questo ci fornirà in output "La stringa "Ciao" ha una lunghezza di 4 caratteri.", confermando ancora una volta il corretto funzionamento del metodo `length()`.

## Approfondimento

Il metodo `length()` di Java è in realtà un'implementazione del campo `length` della classe `String`, che contiene il numero effettivo di caratteri nella stringa. Tuttavia, è importante notare che questo campo è un attributo `final`, il che significa che non è possibile modificarlo e che non è possibile avere una stringa con una lunghezza diversa da quella assegnata originariamente.

Inoltre, è importante ricordare che il metodo `length()` conta tutti i caratteri, incluso lo spazio e i caratteri speciali come il punto esclamativo o la virgola.

## Vedi anche

- [Documentazione ufficiale di Java su String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Tutorial su come gestire le stringhe in Java](https://www.tutorialspoint.com/java/java_strings.htm)
- [Esempi di utilizzo del metodo `length()`](https://examples.javacodegeeks.com/core-java/lang/string/length-method-java-example/)