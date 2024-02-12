---
title:                "Fouten afhandelen"
aliases: - /nl/java/handling-errors.md
date:                  2024-01-28T22:01:40.812406-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
