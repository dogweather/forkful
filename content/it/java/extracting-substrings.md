---
title:    "Java: Estrazione di sottostringhe"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Una delle tecniche più utili nella programmazione Java è l'estrazione di sottostringhe, ovvero la possibilità di estrarre una parte specifica di una stringa. Questo può essere utile in situazioni in cui si desidera manipolare una parte specifica di una stringa o analizzarne una porzione particolare. Vediamo come implementare questa tecnica.

## Come Eseguirlo

Per estrarre una sottostringa in Java, è possibile utilizzare il metodo `substring()` della classe `String`. Questo metodo richiede due parametri: l'indice di inizio della sottostringa e l'indice finale (escluso) della sottostringa desiderata.

Ad esempio, se abbiamo una stringa `"Ciao amici!"` e vogliamo estrarre solo `"amici"`, possiamo utilizzare il seguente codice:

```Java
String s = "Ciao amici!";
String sub = s.substring(5, 10); // indice di inizio: 5, indice finale (escluso): 10
System.out.println(sub); // output: amici
```

Possiamo anche estrarre l'ultima parte di una stringa utilizzando la lunghezza della stessa come indice finale, senza specificare un valore. Ad esempio:

```Java
String s = "Ciao ragazzi!";
String sub = s.substring(5); // indice di inizio: 5, indice finale (escluso): lunghezza della stringa
System.out.println(sub); // output: ragazzi!
```

È importante notare che gli indici in Java iniziano da 0, quindi il primo carattere di una stringa ha indice 0 e l'ultimo carattere ha indice lunghezza della stringa - 1.

## Approfondimento

Oltre alla semplice estrazione di sottostringhe, il metodo `substring()` può essere utilizzato per molte altre operazioni. Ad esempio, è possibile controllare se una parola è presente all'interno di una stringa utilizzando il metodo `contains()` e combinandolo con `substring()`:

```Java
String s = "Ciao amici!";
if (s.contains("amici")) {
    System.out.println("La stringa contiene la parola 'amici'!");
}
```

Possiamo anche utilizzare `substring()` per dividere una stringa in più parti utilizzando un carattere di separazione. Ad esempio, se abbiamo una stringa `"1-2-3-4"` possiamo dividerla in un array di stringhe utilizzando il carattere `-` come separatore:

```Java
String s = "1-2-3-4";
String[] parts = s.split("-");
System.out.println(Arrays.toString(parts)); // output: [1, 2, 3, 4]
```

## Vedi Anche

- Documentazione ufficiale di Java sull'utilizzo del metodo `substring()`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int
- Tutorial su estrazione di sottostringhe in Java: https://www.tutorialspoint.com/java/java_string_substring.htm
- Esempi di utilizzo del metodo `substring()` in situazioni reali: https://www.geeksforgeeks.org/string-substring-methods-java-examples/