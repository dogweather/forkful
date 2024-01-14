---
title:                "Java: Å lese kommandolinje argumenter"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese kommandolinjeargumenter kan være nyttig når du utvikler Java-programmer. Det gir deg muligheten til å gi input til programmet ditt ved hjelp av parameterverdier når du kjører det fra kommandolinjen. Dette kan være spesielt nyttig når du ønsker å lage et program som kan håndtere forskjellige input fra brukeren.

# Hvordan

For å lese kommandolinjeargumenter i Java, må du bruke objektet "args" innenfor main() metoden. Dette er et array av strenger som inneholder alle argumentene som ble gitt ved kjøring av programmet.

Et eksempel på hvordan du kan lese og skrive ut kommandolinjeargumenter i Java:

```Java
public static void main(String[] args) {
    System.out.println("Antall argumenter: " + args.length);
    for (int i = 0; i < args.length; i++) {
        System.out.println("Argument " + (i+1) + ": " + args[i]);
    }
}
```

Kjøring av programmet med argumentene "java" og "programming" vil gi følgende output:

```
Antall argumenter: 2
Argument 1: java
Argument 2: programming
```

# Dypdykk

Det finnes ulike måter å lese og behandle kommandolinjeargumenter på i Java, som for eksempel å bruke Scanner-klassen, String-metoder eller klassen "java.util.Arrays". Det kan også være nyttig å validere argumentene som blir gitt, for eksempel ved å sjekke om antall argumenter er riktig eller om de har riktig format.

Det er også verdt å merke seg at rekkefølgen på argumentene i arrayet "args" vil være den samme som den som ble gitt ved kjøring av programmet.

# Se også

- [Java-tutorial: Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java Dokumentasjon: Arrays](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html)
- [TutorialsPoint: Java - Command Line Input Output](https://www.tutorialspoint.com/java/io/javacommand_line_input.htm)