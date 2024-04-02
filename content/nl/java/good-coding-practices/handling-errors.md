---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:40.812406-07:00
description: "Foutafhandeling betekent code schrijven die anticipeert op en omgaat\
  \ met dingen die misgaan. Programmeurs doen dit om software robuust te maken, waardoor\u2026"
lastmod: '2024-03-13T22:44:50.689845-06:00'
model: gpt-4-0125-preview
summary: "Foutafhandeling betekent code schrijven die anticipeert op en omgaat met\
  \ dingen die misgaan. Programmeurs doen dit om software robuust te maken, waardoor\u2026"
title: Fouten afhandelen
weight: 16
---

## Wat & Waarom?

Foutafhandeling betekent code schrijven die anticipeert op en omgaat met dingen die misgaan. Programmeurs doen dit om software robuust te maken, waardoor crashes en vreemd gedrag worden voorkomen.

## Hoe:

Java gebruikt uitzonderingen om fouten te behandelen. Je omringt risicovolle code met een `try` blok en vangt uitzonderingen op met `catch`. Hier is een simpel voorbeeld:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int resultaat = divide(10, 0);
            System.out.println("Resultaat is: " + resultaat);
        } catch (ArithmeticException e) {
            System.out.println("Oeps, kan niet delen door nul!");
        }
    }

    private static int divide(int teller, int noemer) {
        return teller / noemer;
    }
}
```

Uitvoer:
```
Oeps, kan niet delen door nul!
```

## Uitdieping

Foutafhandeling in Java is geëvolueerd. In de beginperiode waren er geen uitzonderingen; programmeurs controleerden foutcodes. Vervolgens introduceerde Java try-catch blokken, waardoor foutafhandeling eleganter werd.

Alternatieven voor de traditionele `try-catch` zijn onder andere `try-with-resources` voor het automatisch sluiten van bronnen en schonere code, geïntroduceerd in Java 7.

Implementatiedetails doen ertoe. Bijvoorbeeld, het vangen van `Exception` of `Throwable` is meestal slechte praktijk. Het is te breed en maskeert bugs waarvan je misschien niet op de hoogte bent. Houd het bij specifieke uitzonderingen.

## Zie Ook

- De officiële Oracle Java tutorials over uitzonderingen: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Java's `try-with-resources` verklaring documentatie: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effectief Java van Joshua Bloch, voor beste praktijken over uitzonderingen.
