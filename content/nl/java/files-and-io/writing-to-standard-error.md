---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:38.320874-07:00
description: 'Hoe te: Java maakt het schrijven naar stderr eenvoudig met `System.err`.
  Hier is een snelle blik.'
lastmod: '2024-03-13T22:44:50.698994-06:00'
model: gpt-4-0125-preview
summary: Java maakt het schrijven naar stderr eenvoudig met `System.err`.
title: Schrijven naar standaardfout
weight: 25
---

## Hoe te:
Java maakt het schrijven naar stderr eenvoudig met `System.err`. Hier is een snelle blik:

```java
public class StderrExample {
    public static void main(String[] args) {
        System.err.println("Fout: Er is iets misgegaan!");
    }
}
```

Dit uitvoeren geeft je:

```
Fout: Er is iets misgegaan!
```

Opmerking: Terwijl stdout meestal naar de console gaat, kan stderr worden omgeleid naar een bestand of andere bestemming, waardoor foutmeldingen apart blijven.

## Diepere Duik
Historisch gezien is in Unix-achtige systemen stderr bestandsdescriptor 2, onderscheiden van stdout (bestandsdescriptor 1). Dit maakt verschillende afhandelingen en omleidingen mogelijk. Alternatieven voor `System.err` zijn logboekkaders zoals Log4J of SLF4J, die meer functies bieden. In Java wordt stderr geïmplementeerd in de `System` klasse en is het een instantie van `PrintStream`. Het is ongebufferd, wat betekent dat de uitvoer onmiddellijk is.

## Zie Ook
- [Oracle Java Docs - System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Wikipedia - Standaardstromen](https://nl.wikipedia.org/wiki/Standaardstromen)
- [Tutorial over Java Logging](https://www.baeldung.com/java-logging-intro)
