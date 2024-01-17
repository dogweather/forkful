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

## Hva & Hvorfor?
Å lese kommandolinjeargumenter er en måte for programmerere å få innspill fra brukeren mens programmet kjører. Dette hjelper programmerere å bygge dynamiske og interaktive program som kan kommunisere med brukeren på en fleksibel måte.

## Slik gjør du:
For å lese kommandolinjeargumenter i Java, bruker du metoden `getArgs()` fra `main()`-metoden. For eksempel:

```
public static void main(String[] args) {
  if (args.length > 0) {
    // kode for å behandle inntastet argument
    System.out.println("Du skrev: " + args[0]);
  } else {
    // kode for å håndtere ingen argument
    System.out.println("Ingen argument ble oppgitt.");
  }
}
```

La oss si at programmet ditt heter `MinProgram`, og brukeren skriver inn følgende i kommandolinjen:
`java MinProgram Hei verden`

Programmet vil da skrive ut følgende:
`Du skrev: Hei`

## Dypdykk:
Kommandolinjeargumenter har vært en del av programmeringsspråk siden de første operativsystemene dukket opp. Før brukte programmerere å lese Gjenstandsmodell for disse argumentene ved å bruke enkle teknikker som å iterere gjennom argumentlisten. Nåværende backendteknologier som Spring Boot gir standardiserte og moderne måter å håndtere kommandolinjeargumenter på.

En alternativ måte å kommunisere med brukeren på er gjennom et grafisk brukergrensesnitt (GUI). Dette tillater brukere å samhandle med programmet ved å bruke musen og tastaturet, i stedet for å skrive tekst i kommandolinjen. Bruk av kommandolinjeargumenter kan være mer effektivt for programmer som har et spesifikt formål og gjentakende oppgaver. 

Det finnes også ulike metoder for å håndtere feil og uventet brukerinnsats når man leser kommandolinjeargumenter. Det er viktig å ha god feilhåndtering i programmer for å skape en brukervennlig og pålitelig opplevelse for brukeren.


## Se også:
* <https://www.baeldung.com/java-command-line-arguments>
* <https://www.geeksforgeeks.org/command-line-arguments-in-java/>