---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:10.589271-07:00
description: "String interpolatie stelt je in staat om variabelen direct in strings\
  \ in te voegen. Het maakt code schoner en gemakkelijker te lezen door onhandige\u2026"
lastmod: '2024-03-13T22:44:50.666689-06:00'
model: gpt-4-0125-preview
summary: "String interpolatie stelt je in staat om variabelen direct in strings in\
  \ te voegen. Het maakt code schoner en gemakkelijker te lezen door onhandige\u2026"
title: Een string interpoleren
weight: 8
---

## Wat & Waarom?
String interpolatie stelt je in staat om variabelen direct in strings in te voegen. Het maakt code schoner en gemakkelijker te lezen door onhandige stringconcatenatie te vermijden.

## Hoe te gebruiken:
Java introduceerde `String.format()` voor interpolatie:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Alice";
    int points = 1337;
    String groet = String.format("Hoi, %s! Je hebt %d punten.", user, points);
    System.out.println(groet);
  }
}
```
Voorbeeld van output:
```
Hoi, Alice! Je hebt 1337 punten.
```

Voor meer moderne interpolatie sinds Java 15, gebruiken we tekstblokken en `formatted()`:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Bob";
    double rekeningsaldo = 1234.56;
    String bericht = """
      Beste %s,
      Je huidige saldo is $%.2f.
      """.formatted(user, rekeningsaldo);
    System.out.println(bericht);
  }
}
```
Voorbeeld van output:
```
Beste Bob,
Je huidige saldo is $1234.56.
```

## Diepere Duik
Voor interpolatie vertrouwde Java op concatenatie: `String groet = "Hallo, " + user + "!";`. Omslachtig en foutgevoelig, zeker naarmate strings complexer werden.

Historisch gezien hadden talen zoals Perl en PHP interpolatie. Java haalde veel later in. `String.format()` en `PrintStream.printf()` bieden vergelijkbare functionaliteit, met behulp van formatteringsspecificaties die Java vertellen hoe om te gaan met variabelen.

Alternatieven? Naast `String.format()`, hebben we `MessageFormat` en `StringBuilder`, maar die zijn niet zo geschikt voor basisinterpolatie. Sinds Java 15 hebben tekstblokken het vereenvoudigd om meerlijnige strings te gebruiken en `formatted()` toegevoegd om interpolatie direct op zijn plaats te stroomlijnen.

Wat betreft implementatie, gebruikt `String.format()` `Formatter`, een robuuste motor met veel formatteringsopties. Maar let op, complexe strings kunnen de prestaties van je app negatief beïnvloeden als je niet voorzichtig bent.

## Zie Ook
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Formatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [JEP 378: Tekstblokken (Definitief)](https://openjdk.java.net/jeps/378)
