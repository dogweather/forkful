---
date: 2024-01-20 17:56:22.556454-07:00
description: "Lesing av kommandolinjeargumenter lar deg ta imot data n\xE5r du starter\
  \ et Java-program. Programmererne gj\xF8r dette for \xE5 gj\xF8re programmene fleksible\
  \ og\u2026"
lastmod: 2024-02-19 22:04:59.927426
model: gpt-4-1106-preview
summary: "Lesing av kommandolinjeargumenter lar deg ta imot data n\xE5r du starter\
  \ et Java-program. Programmererne gj\xF8r dette for \xE5 gj\xF8re programmene fleksible\
  \ og\u2026"
title: Lese kommandolinjeargumenter
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter lar deg ta imot data når du starter et Java-program. Programmererne gjør dette for å gjøre programmene fleksible og tilpasse kjøringen uten å endre koden.

## Hvordan:
```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Hei, " + args[0] + "!");
        } else {
            System.out.println("Hei, ukjente bruker!");
        }
    }
}
```
Kjør programmet:
```
$ java CommandLineExample Verden
Hei, Verden!
```

Uten argumenter:
```
$ java CommandLineExample
Hei, ukjente bruker!
```

## Dypdykk
Kommandolinjeargumenter er like gamle som kommandolinjen selv – tenk 60-tallet. Alternativene inkluderer bruk av miljøvariabler, konfigurasjonsfiler eller interaktiv input. Når Java-programmer starter, lagres argumenter i `String`-arrayet `args` i `main`-metoden. `args[0]` er det første argumentet, `args[1]` det andre, og så videre. Fordi `args` er et array, kan du bruke standard `length`-egenskapen for å finne ut hvor mange argumenter som er passert.

## Se Også
- Oracles offisielle dokumentasjon på kommandolinjeargumenter: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html
- `java.util.Scanner` for å lese brukerinput: https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html
- Apache Commons CLI for avansert argumenthåndtering: https://commons.apache.org/proper/commons-cli/
