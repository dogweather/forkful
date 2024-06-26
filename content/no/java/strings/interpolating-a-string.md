---
date: 2024-01-20 17:51:09.582336-07:00
description: "How to: (Hvordan:) I Java 15 ble det introdusert en forh\xE5ndsvisningsfunksjon\
  \ kalt text blocks, som gj\xF8r string interpolasjon enklere. Her er et eksempel."
lastmod: '2024-04-05T21:53:41.633183-06:00'
model: gpt-4-1106-preview
summary: "(Hvordan:) I Java 15 ble det introdusert en forh\xE5ndsvisningsfunksjon\
  \ kalt text blocks, som gj\xF8r string interpolasjon enklere."
title: Interpolering av en streng
weight: 8
---

## How to: (Hvordan:)
I Java 15 ble det introdusert en forhåndsvisningsfunksjon kalt text blocks, som gjør string interpolasjon enklere. Her er et eksempel:

```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String user = "Einar";
        int points = 120;
        
        // Bruk av String.format() for interpolasjon
        String message = String.format("Hei, %s! Du har oppnådd %d poeng!", user, points);
        System.out.println(message); // Output: Hei, Einar! Du har oppnådd 120 poeng!
    }
}
```

## Deep Dive (Dypdykk)
Historisk brukte Java `%`-formatsspesifikatorer og `String.format()` for interpolasjon. I nyere versjoner, fra Java 15 med JEP 355, har vi noe som ligner mer på ekte string interpolasjon gjennom text blocks. Alternativer til `String.format()` inkluderer å bruke `+` for å konkatenere eller `StringBuilder` for mer komplekse operasjoner og bedre ytelse. Hvordan det implementeres er viktig for minnebruk og ytelse - `StringBuilder` er ofte det beste valget for lange eller kompliserte strenger.

## See Also (Se Også)
- [Java String.format documentation](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Formatter.html)
- [JEP 355: Text Blocks (Second Preview)](https://openjdk.java.net/jeps/355)
- [Oracle's Java Tutorials – Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
