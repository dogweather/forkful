---
title:                "Lese kommandolinjeargumenter"
html_title:           "Java: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har laget et Java-program som må ta inn variabler eller verdier fra brukeren, har du kanskje lagt merke til at dette vanligvis gjøres fra en brukergrensesnitt. Men hva om du vil kunne gi input til programmet ditt direkte fra kommandolinjen? Dette er når det er nyttig å vite hvordan man leser kommandolinjeargumenter i Java.

## Hvordan

For å lese kommandolinjeargumenter i Java, må du bruke `args` -parameteren i `main` -metoden din. Denne parameteren er en array av `String` -objekter som inneholder alle argumentene som ble gitt til programmet når det ble startet. Lat oss se på et enkelt eksempel:

```Java
public class Main {
  public static void main(String[] args) {
    for (String arg : args) {
      System.out.println(arg);
    }
  }
}
```

La oss si at vi kompilerer og kjører dette programmet med følgende kommandolinjeinndata:

```Java
java Main hello world !
```

Vi vil da få følgende utskrift:

```
hello 
world 
!
```

Som du kan se, blir hvert argument lagt til i `args` -arrayen, og vi kan deretter gjøre hva vi vil med dem, for eksempel skrive dem ut til konsollen som i eksempelet ovenfor.

## Dypdykk

I tillegg til å lese argumentene som blir gitt til programmet, kan du også sjekke hvor mange argumenter som er gitt ved å bruke `args.length` -metoden. Du kan også endre rekkefølgen på argumentene ved å bruke indekser på `args` -arrayen.

Det er imidlertid viktig å merke seg at kommandolinjeargumenter alltid er av typen `String`, så hvis du trenger å bruke disse verdiene som numeriske eller booleske verdier, må du konvertere dem først.

## Se også

- [Official documentation for Command-Line Arguments in Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorialspoint's guide to Command-Line Arguments in Java (in Norwegian)](https://www.tutorialspoint.com/norskprogrammering/command_line_arguments_in_java.htm)