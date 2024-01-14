---
title:    "Java: Unire stringhe"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un processo essenziale in programmazione Java. In sostanza, ci consente di unire diverse stringhe per creare una nuova stringa. È utile in molte situazioni, ad esempio per creare output più complessi, formattare testi o costruire URL.

## Come fare

Per concatenare stringhe in Java, possiamo utilizzare l'operatore `+` o il metodo `concat()`. Vediamo un esempio di entrambi i metodi utilizzando la classe `String`:

```Java
String before = "Buon";
String after = "giorno!";
String greeting = before + " " + after;
System.out.println(greeting); // Output: Buon giorno!

String message = before.concat(" pomeriggio!");
System.out.println(message); // Output: Buon pomeriggio!
```

In entrambi i casi, abbiamo creato una nuova stringa unendo le stringhe esistenti `before` e `after`. Tuttavia, l'utilizzo dell'operatore `+` è più leggibile e intuitivo. Possiamo anche concatenare più di due stringhe utilizzando una catena di operatori `+` o metodi `concat()`.

## Approfondimento

Per comprendere meglio il funzionamento della concatenazione di stringhe in Java, è importante conoscere alcune caratteristiche del linguaggio. In particolare, dobbiamo tenere a mente che le stringhe sono immutabili, il che significa che una volta create non possono essere modificate.

Quando utilizziamo l'operatore `+` per concatenare stringhe, in realtà stiamo creando una nuova istanza di `String` ogni volta. Ciò potrebbe influire sulle prestazioni del nostro programma, soprattutto se stiamo manipolando molte stringhe. In questi casi, è più efficiente utilizzare il metodo `StringBuilder` o `StringBuffer` per concatenare stringhe mutabili.

Inoltre, è importante prestare attenzione alle performance quando si concatenano grandi quantità di stringhe. In questi casi, l'utilizzo del `+` o del metodo `concat()` potrebbe essere meno efficiente rispetto alla classe `StringJoiner` introdotta in Java 8.

## Vedi anche

- [Documentazione ufficiale di Java sulla classe String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial su come utilizzare l'operatore + e il metodo concat() per concatenare stringhe](https://www.geeksforgeeks.org/concatenating-strings-in-java/)
- [Approfondimento sull'uso di StringBuilder e StringJoiner per le operazioni di concatenazione](https://www.baeldung.com/java-string-concatenation-performance)