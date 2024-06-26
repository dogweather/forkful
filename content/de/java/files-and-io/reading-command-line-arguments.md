---
date: 2024-01-20 17:56:01.313692-07:00
description: "How to: Befehlszeile zum Ausf\xFChren: `java CommandLineReader Hallo\
  \ Welt` Ausgabe."
lastmod: '2024-04-05T21:53:55.660999-06:00'
model: gpt-4-1106-preview
summary: "Befehlszeile zum Ausf\xFChren."
title: Lesen von Kommandozeilenargumenten
weight: 23
---

## How to:
```java
public class CommandLineReader {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Erhaltene Argumente:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Keine Argumente übergeben.");
        }
    }
}
```
Befehlszeile zum Ausführen: `java CommandLineReader Hallo Welt`
Ausgabe:
```
Erhaltene Argumente:
Hallo
Welt
```

## Deep Dive
Kommandozeilenargumente sind so alt wie die Kommandozeile selbst. In Java sind sie direkt über das `String[] args` des `main`-Verfahrens verfügbar. Alternativen zum Basis-`String[] args`-Mechanismus beinhalten die Verwendung von Bibliotheken wie Apache Commons CLI und JCommander, die das Parsen und Verwalten der Argumente vereinfachen. Wichtig beim Implementieren ist, dass diese Argumente als unveränderbare `String`-Werte vorliegen und dass die Validierung und das Parsing der Eingaben essentiell sind, um Fehlern zu vermeiden.

## See Also
- [Oracle's Java Documentation on Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
- [JCommander Official Site](http://jcommander.org/)
