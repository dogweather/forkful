---
title:                "Java: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese kommandolinjeargumenter er en viktig ferdighet for enhver Java-programmerer. Det lar deg enkelt gi input til programmet ditt mens det kjører, og gjør det mulig å tilpasse programmet for ulike scenarier.

# Hvordan

Det er flere måter å lese kommandolinjeargumenter på i Java, men den enkleste og vanligste måten er å bruke `args` parameteren i `main`-metoden. Her er et eksempel på hvordan dette kan gjøres:

```Java
public class CommandLineArgsExample {
  public static void main(String[] args) {
    // Itererer gjennom alle argumentene og skriver dem ut
    for (String arg : args) {
      System.out.println(arg);
    }
  }
}
```
Hvis vi kjører dette programmet med følgende kommandolinjeinput: `java CommandLineArgsExample arg1 arg2 arg3`, vil output bli:

```
arg1
arg2
arg3
```

Du kan også bruke `args`-parameteren til å ta imot spesifikke argumenter, for eksempel:

```Java
public class SpecificArgsExample {
  public static void main(String[] args) {
    // Sjekker om det tredje argumentet er "hello"
    if (args[2].equals("hello")) {
      System.out.println("Hello there!");
    } else {
      System.out.println("Invalid input!");
    }
  }
}
```
Hvis vi nå kjører dette programmet med kommandolinjeinput: `java SpecificArgsExample arg1 arg2 hello`, vil output bli:

```
Hello there!
```

# Dypdykk

Når du leser kommandolinjeargumenter, er det viktig å være oppmerksom på at det er grenser for hvor mange argumenter du kan sende inn. Dette kan variere avhengig av operativsystemet ditt og din JDK-versjon. Det er også viktig å sørge for at argumentene er på riktig format, for eksempel hvis du forventer et tall som input, må du konvertere det til `int` eller `double` før du bruker det.

En annen viktig ting å huske på er at rekkefølgen på argumentene betyr noe. Hvis du for eksempel forventer to tall som input og bruker dem for å utføre en matematisk operasjon, må du sørge for at de blir gitt i korrekt rekkefølge.

# Se Også

- [Java Main Method: How to Read Command Line Arguments](https://www.javatpoint.com/java-main-method)
- [Passing command line arguments to a Java program](https://www.geeksforgeeks.org/command-line-arguments-in-java/)
- [Java Read Command Line Arguments Examples](https://www.codejava.net/java-se/reading-command-line-arguments-in-java-examples)