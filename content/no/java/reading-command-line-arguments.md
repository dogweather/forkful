---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinjeargumenter lar oss gi parametere ved oppstart av et Java-program. Dette er nyttig for fleksibilitet, som å bestemme filstier eller justere programoppførsel.

## Hvordan gjøre det:

For å lese kommandolinjeargumenter i Java, brukes `main()`-metoden. Denne tar en parameter som er en array av strenger (`String[] args`), hvor hver streng representerer et argument.

```Java
public class HelloWorld {
    public static void main(String[] args) {
        for(String arg : args) {
            System.out.println("Argument: " + arg);
        }
    }
}
```

Hvis du kjører dette programmet med `java HelloWorld Hei Verden`, vil utdataene være:

```Java
Argument: Hei
Argument: Verden
```

## Dypdykk

Kommandolinjeargumenter har vært en del av mange programmeringsspråk i årevis, inkludert C og Perl, langt før Java.

Alternativer til å bruke disse argumentene inkluderer å lese data fra en fil, brukerinput eller hardkoding av verdier, men ingen av disse tilbyr samme fleksibilitet og enkelhet.

Et viktig detalj er at `main()`-metoden ikke vet antallet argumenter på forhånd. Du bruker `args.length` for å finne antall argumenter.

## Se Også

For mer informasjon om kommandolinje-argumenter:

- Oracle Docs: https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html
- Java Tutorials: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html