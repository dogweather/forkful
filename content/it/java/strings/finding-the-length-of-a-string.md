---
date: 2024-01-20 17:47:44.852128-07:00
description: 'How to: Ecco come farlo in Java. Usiamo il metodo `.length()` di un
  oggetto `String`.'
lastmod: '2024-03-13T22:44:43.300094-06:00'
model: gpt-4-1106-preview
summary: Ecco come farlo in Java.
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to:
Ecco come farlo in Java. Usiamo il metodo `.length()` di un oggetto `String`:

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String saluto = "Ciao, mondo!";
        int lunghezza = saluto.length();
        System.out.println("La lunghezza della stringa è: " + lunghezza);
    }
}
```

Output:
```
La lunghezza della stringa è: 12
```

## Deep Dive
In Java, il metodo `.length()` esiste da sempre: fa parte delle specifiche originarie del linguaggio. È un metodo rapido e diretto, senza alternative standard nella libreria Java. Tuttavia, in contesti come arrays di byte o buffer, potresti incontrare metodi come `.size()` o `.capacity()`.

Internamente, una `String` in Java è un'array di caratteri, quindi `.length()` ritorna semplicemente la dimensione di quest'array. Da quando Java ha aggiunto il supporto per Unicode tramite i tipi di caratteri UTF-16, il concetto di lunghezza può essere ingannevole: un carattere Unicode può essere rappresentato da più caratteri Java se si estende oltre il Basic Multilingual Plane.

## See Also
- [Java String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Understanding Java Strings and the String Pool](https://www.baeldung.com/java-string-pool)
- [Unicode in Java: How to Handle It](https://www.oracle.com/technical-resources/articles/javase/supplementary.html)
