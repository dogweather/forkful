---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:05.201440-07:00
description: "De lengte van een string vinden betekent uitzoeken hoeveel tekens deze\
  \ bevat. Programmeurs doen dit vaak om invoer te valideren, door karakters te lussen,\u2026"
lastmod: '2024-03-13T22:44:50.672186-06:00'
model: gpt-4-0125-preview
summary: De lengte van een string vinden betekent uitzoeken hoeveel tekens deze bevat.
title: De lengte van een string vinden
weight: 7
---

## Wat & Waarom?
De lengte van een string vinden betekent uitzoeken hoeveel tekens deze bevat. Programmeurs doen dit vaak om invoer te valideren, door karakters te lussen, of tekst uit te lijnen.

## Hoe:
Java-strings hebben een `length()` methode. Roep het aan, en je krijgt het aantal tekens. Eenvoudig.

```java
public class StringLengthExample {
    public static void main(String[] args) {
        String groet = "Hallo, Wereld!";
        int lengte = groet.length();

        System.out.println("De lengte van de string is: " + lengte);
        // Uitvoer: De lengte van de string is: 13
    }
}
```

## Diepgaand
De `length()` methode dateert van de vroegste Java-versies, waardoor het een langdurig onderdeel van de `String` klasse is. Het is eenvoudig maar essentieel. Intern is een `String` in Java ondersteund door een tekenreeks, waarbij de `length()` methode de grootte van deze reeks retourneert. Cruciaal is dat Java-strings onveranderlijk zijn, dus eenmaal gecreëerd, verandert de lengte niet, wat de methode snel en betrouwbaar maakt.

Alternatieven? Wel, anders dan je eigen functie maken om karakters te tellen (onnodig en niet-presterend), niet echt. Houd er rekening mee dat `length()` het aantal `char` eenheden retourneert, niet noodzakelijkerwijs codepunten. Voor Unicode-tekens die niet passen in de standaard 16-bit `char` grootte, overweeg `codePointCount()` te gebruiken als je rekening moet houden met aanvullende karakters.

## Zie Ook
Duik dieper of verken gerelateerde onderwerpen:
- [Java String Documentatie](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Character Class Docs](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/Character.html) voor meer begrip over Unicode, karakters, en codepunten.
- [Oracle's Java Tutorials](https://docs.oracle.com/javase/tutorial/java/data/strings.html) voor een breder begrip van strings in Java.
