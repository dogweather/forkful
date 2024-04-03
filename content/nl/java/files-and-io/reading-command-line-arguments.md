---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.938959-07:00
description: "Command line argumenten lezen in Java is het binnenhalen van input die\
  \ door gebruikers wordt gegeven wanneer ze jouw programma vanaf een console starten.\u2026"
lastmod: '2024-03-13T22:44:50.698034-06:00'
model: gpt-4-0125-preview
summary: Command line argumenten lezen in Java is het binnenhalen van input die door
  gebruikers wordt gegeven wanneer ze jouw programma vanaf een console starten.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe:
Java pikt de command line argumenten die je geeft op met de `main` methode. Kijk naar dit hapklare voorbeeld:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        // Laten we de command line argumenten uitprinten
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

Start je terminal, compileer met `javac CommandLineExample.java`, en run met `java CommandLineExample Deze Zijn Command Line Argumenten`. Hier is je output:

```
Deze
Zijn
Command
Line
Argumenten
```

## Diepgaand
Voortkomend uit C, zijn command line argumenten een basiscomponent sinds de duistere tijden van programmering—denk aan ponskaarten en timesharing. Java heeft deze functionaliteit om goede redenen geërfd. Het is basaal, veelzijdig, en past bij een reeks situaties.

Zeer alternatief? Zeker, er zijn genoeg mogelijkheden. Bibliotheken zoals JCommander of Apache Commons CLI versterken je parseringskracht. Ze behandelen complexere scenario's met finesse.

Onder de motorkap, pikt Java's `main` methode een `String` array—`args` op. In de virtuele machine run, wanneer je `java Klassenaam` intikt, wat volgt zijn jouw inputs, netjes opgeslagen in `args`.

## Zie Ook:
- Voor een opfrisser over de basis: [Oracle's officiële Java tutorials](https://docs.oracle.com/javase/tutorial/)
- Duik in JCommander voor complexe parsing: [JCommander GitHub](https://github.com/cbeust/jcommander)
- Verken Apache Commons CLI: [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
