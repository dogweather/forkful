---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:16.059142-07:00
description: 'Hoe te: De `String` klasse in Java heeft een handige `toLowerCase()`
  methode die het zware werk voor je doet. Bekijk dit eenvoudige gebruik.'
lastmod: '2024-03-13T22:44:50.667642-06:00'
model: gpt-4-0125-preview
summary: De `String` klasse in Java heeft een handige `toLowerCase()` methode die
  het zware werk voor je doet.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe te:
De `String` klasse in Java heeft een handige `toLowerCase()` methode die het zware werk voor je doet. Bekijk dit eenvoudige gebruik:

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String origineel = "Java ROCKS!";
        String lowerCased = origineel.toLowerCase();
        System.out.println(lowerCased);
    }
}
```

Uitvoer:

```
java rocks!
```

Dat is het. De string krijgt zijn volume gedraaid naar een relaxte kleine-lettervorm.

## Diepgaande Duik
Er was eens een tijd waarin het omgaan met tekst een lastige zaak was. Verschillende talen, verschillende hoofdletters, computersystemen die in verwarring schreeuwden. Java, die in de jaren '90 op het toneel kwam, wilde dingen makkelijker maken. De `toLowerCase()` methode is al sinds de vroege dagen onderdeel van Java's `String` klasse.

Maar er zit wat cools onder de motorkap. Je vraagt je misschien af waarom `toLowerCase()` zelfs nodig is. Het ding is, niet alle culturen definiëren "kleine letters" op dezelfde wijze. De methode is gevoelig voor locale, gebruik makend van je systeem's standaard locale, of je kunt er een specificeren met `toLowerCase(Locale locale)`.

Hier is nog een draai: talen met meer versierde scripts, zoals het Turks, hebben speciale "puntloze" i-karakters die een reguliere naar-kleine-letter-omzetting uit het raam kunnen gooien. Daarom biedt Java de optie om nauwkeurig met tekenconversies om te gaan.

Alternatieven? Zeker, je zou door de string kunnen rommelen met een `for` lus, tekens handmatig verwisselend. Maar waarom het wiel opnieuw uitvinden als Java je gedekt heeft?

Ook, dit kan sommigen verrassen: strings in Java zijn onveranderlijk. Wanneer je `toLowerCase()` gebruikt, wijzig je niet de originele string, je creëert een geheel nieuwe, vest en al.

## Zie Ook
Bekijk deze bronnen om je string-spel te verbeteren:

- Java String API: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- Java Locale Class: [](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Locale.html)
- Unicode Case Mappings: [](https://unicode.org/reports/tr21/)

En voor de grittige details over de Unicode Standaard:

- Het Unicode Consortium: [](https://unicode.org/)
