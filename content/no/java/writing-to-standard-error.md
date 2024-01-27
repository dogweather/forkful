---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard error (stderr) betyr å sende feilmeldinger og diagnostikk til en spesifikk utkanal. Programmerere gjør dette for å skille vanlig programdata fra feil og logger, noe som hjelper med feilsøking og logging.

## Slik gjør du:
Java har allerede en innebygd måte å håndtere stderr på, som er `System.err`. Her er et enkelt eksempel:

```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("Dette er en normal melding til stdout.");
        System.err.println("Dette er en feilmelding sendt til stderr.");
    }
}
```

Kjører du dette, vil du se begge meldingene, men feilmeldingen er kanskje i rødt eller annen farge, avhengig av konsollet.

## Dypdykk
Historisk har skille mellom stdout og stderr latt utviklere og brukere av terminalen omadressere disse strømmene separat. Dette kan være nyttig i skripting og når output fra programmer skal i ulike loggfiler. En alternativ måte å håndtere feil på kunne være å kaste unntak, men stderr er nyttig fordi det ikke forstyrrer programflyten. Implisitt bruker `System.err` `PrintStream`-klassen, som har flere metoder for å formatere og skrive data.

## Se Også
- [Oracle Java Docs - System.err](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/System.html#err)
- [Stack Overflow - When to use System.out, System.err, and System.in](https://stackoverflow.com/questions/3131865/when-to-use-system-out-system-err-and-system-in)
