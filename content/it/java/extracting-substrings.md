---
title:    "Java: Estrazione di sottostringhe"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché

Molte volte, mentre programmi in Java, potresti aver bisogno di ottenere una parte di una stringa più grande. Questo processo è comunemente chiamato "estrazione di sottostringhe" ed è utile per estrarre informazioni specifiche dalle stringhe.

## Come Fare

Per estrarre una sottostringa da una stringa in Java, utilizziamo il metodo `substring()` della classe `String`. Questo metodo richiede due parametri: l'indice iniziale e l'indice finale della sottostringa che si desidera estrarre.

```Java
String str = "Ciao a tutti!";
String sub = str.substring(5, 8);

System.out.println("La sottostringa è: " + sub);
```

Output:
```
La sottostringa è: tutti
```

## Approfondimento

Il primo parametro del metodo `substring()` è l'indice iniziale della sottostringa, che inizia da 0 per la prima lettera della stringa originale. Il secondo parametro è l'indice finale, che è esclusivo, quindi la sottostringa viene selezionata fino all'indice precedente.

Inoltre, è possibile utilizzare il metodo `substring()` per estrarre una parte di una stringa fino alla fine utilizzando un solo parametro, come nel seguente esempio:

```Java
String str = "Programmiamo in Java";
String sub = str.substring(15);

System.out.println("La sottostringa è: " + sub);
```

Output:
```
La sottostringa è: Java
```

## Vedi Anche

- Documentazione ufficiale di Java sulla classe `String`: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Tutorial su come utilizzare il metodo `substring()`: https://www.tutorialspoint.com/java/java_string_substring.htm
- Esempi pratici di estrazione di sottostringhe in Java: https://www.geeksforgeeks.org/substring-java/