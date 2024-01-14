---
title:                "Java: Unione di stringhe"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché
Molti linguaggi di programmazione permettono l'unione di più stringhe, ma perché dovremmo farlo? Concatenare le stringhe può sembrare un concetto semplice, ma ha molte applicazioni utili nella programmazione Java.

## Come fare
Per unire due o più stringhe in Java, possiamo utilizzare l'operatore "+", che funziona come un simbolo di addizione per le stringhe. Ad esempio:

```Java
String saluto = "Ciao";
String nome = "Marco";
System.out.println(saluto + " " + nome);
```
L'output sarà: "Ciao Marco".

Possiamo anche utilizzare il metodo concat() della classe String, che ha la stessa funzione dell'operatore "+". Ad esempio:

```Java
String saluto = "Ciao";
String nome = "Marco";
System.out.println(saluto.concat(" ").concat(nome));
```
L'output sarà lo stesso: "Ciao Marco".

## Approfondimento
Nella programmazione Java, concatenare le stringhe può essere utile per la formattazione dei dati, la creazione di messaggi dinamici o la visualizzazione di output personalizzati. Possiamo anche concatenare più di due stringhe in una singola linea di codice, aumentando così l'efficienza e la leggibilità del nostro codice.

È importante notare che le stringhe in Java sono immutabili, il che significa che non possono essere modificate una volta create. Quando concateniamo le stringhe, in realtà ne stiamo creando una nuova ogni volta. Perciò, se si prevede di concatenare molte stringhe, può essere più efficiente utilizzare la classe StringBuilder, che crea una stringa modificabile e può offrire prestazioni migliori.

## Vedi anche
- [Documentazione ufficiale di Java su concatenare stringhe](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)
- [Tutorial su concat() e StringBuilder](https://www.geeksforgeeks.org/concat-vs-concatenate-methods-java/)
- [Esempi pratici di concatenazione di stringhe in Java](https://www.baeldung.com/string-concatenation-performance-java)