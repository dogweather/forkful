---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:57.082121-07:00
description: 'Hoe te: Het extraheren van een substring in Java is eenvoudig met behulp
  van de methode `substring`. Hier is hoe je dat doet.'
lastmod: '2024-03-13T22:44:50.669813-06:00'
model: gpt-4-0125-preview
summary: Het extraheren van een substring in Java is eenvoudig met behulp van de methode
  `substring`.
title: Substrings extraheren
weight: 6
---

## Hoe te:
Het extraheren van een substring in Java is eenvoudig met behulp van de methode `substring`. Hier is hoe je dat doet:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String fullString = "Hallo, wereld!";

        // Extractie van index 7 tot het einde van de string
        String sub1 = fullString.substring(7);
        System.out.println(sub1); // Uitvoer: wereld!

        // Extractie van index 0 tot index 4 (5 is niet inbegrepen)
        String sub2 = fullString.substring(0, 5);
        System.out.println(sub2); // Uitvoer: Hallo
    }
}
```

**Onthoud**: In Java begint de indexering van strings bij 0.

## Diepere Duik
De methode `substring` bestaat al sinds de vroege dagen van Java en biedt een eenvoudige manier om delen van een string te krijgen. In oudere versies van Java zou `substring` de oorspronkelijke karakterarray delen, wat kon leiden tot geheugenlekken als de oorspronkelijke string groot was en de substring lange tijd werd bewaard. Sinds Java 7 update 6 creëert `substring` een nieuwe string, zodat de oude kan worden opgehaald voor garbage collection als deze niet elders wordt gebruikt.

Ook, voordat je naar `substring` grijpt, overweeg of je `split`, `replace`, of regex-hulpprogramma's kunt gebruiken voor meer complexe scenario's. Intern gebruikt `substring` in Java methoden van de `String`-klasse die arrays kopiëren - efficiënt, maar niet iets waar je directe controle over hebt.

## Zie Ook
- Voor een volledig beeld van wat je met strings in Java kunt doen, neem een kijkje naar de documentatie van de `String` klasse: [String (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- Duik in meer complexe stringmanipulatie? De klassen `Pattern` en `Matcher` zijn je vrienden: [Pattern (Java SE 15 & JDK 15)](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html)
- Een handleiding over het gebruik van reguliere expressies in Java: [Reguliere Expressies](https://docs.oracle.com/javase/tutorial/essential/regex/)

Of het nu is voor een snelle trim of voor complexe gegevensextractie, de functies die je nodig hebt zijn er. Houd je gereedschapskist goed begrepen en klaar voor gebruik.
