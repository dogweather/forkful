---
title:                "Java: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

L'operazione di concatenazione di stringhe è fondamentale nella programmazione Java. Essa ci permette di unire due o più stringhe in una singola stringa, consentendo di creare nuove parole o frasi all'interno del nostro codice.

## Come fare

Per concatenare delle stringhe in Java, possiamo utilizzare l'operatore "+", che unisce due stringhe in una sola. Ecco un esempio:

```java
String saluto = "Ciao";
String nome = "Alice";
String messaggio = saluto + " " + nome; // messaggio diventa "Ciao Alice"
System.out.println(messaggio); // output: Ciao Alice
```

Possiamo anche concatenare una stringa a un valore numerico, ma dobbiamo prima convertire il numero in una stringa utilizzando il metodo `String.valueOf()`. Ad esempio:

```java
int numero = 10;
String testo = "Il numero è: " + String.valueOf(numero); // testo diventa "Il numero è: 10"
System.out.println(testo); // output: Il numero è: 10
```

E se volessimo concatenare più di due stringhe? Possiamo farlo uno di seguito all'altro utilizzando più volte l'operatore "+", oppure possiamo utilizzare il metodo `concat()` della classe String. Ecco un esempio:

```java
String stringa1 = "Questo è";
String stringa2 = "un esempio";
String stringa3 = "di concatenazione";
String output = stringa1.concat(" ").concat(stringa2).concat(" ").concat(stringa3); // output diventa "Questo è un esempio di concatenazione"
System.out.println(output); // output: Questo è un esempio di concatenazione
```

## Approfondimenti

È importante notare che l'operazione di concatenazione di stringhe in Java può diventare inefficiente se utilizzata su un grande numero di stringhe, in quanto ogni volta che viene utilizzato l'operatore "+", una nuova stringa viene creata e allocata in memoria. In questi casi, sarebbe più efficiente utilizzare la classe `StringBuilder`, che ci consente di concatenare stringhe in modo più efficiente. Ad esempio:

```java
StringBuilder sb = new StringBuilder();
sb.append("Ciao");
sb.append(" ");
sb.append("Alice");
String messaggio = sb.toString(); // messaggio diventa "Ciao Alice"
```

Inoltre, esistono anche altre classi come `StringBuffer` e `StringJoiner` che ci permettono di concatenare stringhe in modo più efficiente rispetto all'uso dell'operatore "+". È importante tenerne conto soprattutto quando si lavora con grandi quantità di dati.

## Vedi anche

- [Documentazione ufficiale di Oracle su String in Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tutorial su come utilizzare la classe StringBuilder in Java](https://www.baeldung.com/java-string-builder)
- [Differenze tra StringBuilder, StringBuffer e StringJoiner in Java](https://www.geeksforgeeks.org/stringbuffer-stringbuilder-and-stringjoiner-in-java/)