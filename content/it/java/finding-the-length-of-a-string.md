---
title:    "Java: Calcolare la lunghezza di una stringa"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Trovare la lunghezza di una stringa è una delle operazioni più comuni necessarie nella programmazione. Sapere quanto è lunga una stringa può essere utile per manipolarla, stamparla o confrontarla con altre stringhe.

## Come Fare
Per trovare la lunghezza di una stringa in Java, possiamo utilizzare il metodo `length()` della classe `String`. Vediamo un esempio di codice che mostra come utilizzarlo:

```Java
String stringa = "Ciao Mondo!";

System.out.println("La lunghezza della stringa è: " + stringa.length());
```

Output:
```
La lunghezza della stringa è: 11
```

Come possiamo notare, il metodo `length()` restituirà il numero di caratteri presenti nella stringa, spazi inclusi. Possiamo anche utilizzare questo metodo in combinazione con altri metodi della classe `String` per ottenere informazioni più precise sulla lunghezza della stringa.

## Approfondimento
Nel linguaggio di programmazione Java, le stringhe sono immutabili, il che significa che non possono essere modificate una volta create. Ciò rende il calcolo della lunghezza di una stringa molto efficiente, poiché non è necessario scorrere la stringa per contare tutti i caratteri, ma solo leggere il suo campo di lunghezza interno.

Inoltre, è importante tenere presente che il metodo `length()` restituirà un `int`, il che significa che la lunghezza massima di una stringa in Java è di 2.147.482.647 caratteri.

## Vedi Anche
- Java String length: https://www.w3schools.com/java/ref_string_length.asp
- Java String class: https://www.geeksforgeeks.org/java-string-class/
- Immutability in Java: https://www.baeldung.com/java-immutable-strings