---
title:    "Java: Å lese kommandolinje-argumenter"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter kan være en nyttig ferdighet å ha som Java-utvikler. Det kan hjelpe deg med å lage mer dynamiske og tilpasningsdyktige programmer, og kan også forenkle debugging-prosessen. Hvis du er nysgjerrig på hvordan du kan gjøre dette, les videre!

## Hvordan

For å lese kommandolinjeargumenter i Java, kan du bruke funksjonen `args[]` som er en del av `main()`-metoden. Dette vil tillate deg å hente og lagre argumentene som brukeren skriver inn når de kjører programmet ditt.

```Java
public static void main(String[] args) {
    //sjekker om det er minst ett argument skrevet inn
    if (args.length > 0) {
        //løper gjennom hvert argument og skriver det til konsollen
        for (int i = 0; i < args.length; i++) {
            System.out.println("Argument " + (i+1) + ": " + args[i]);
        }
    }
}
```

La oss si at du har kompilert og kjører programmet ditt med argumentene `java program.java argument1 argument2`. Outputen du vil få er:

`Argument 1: argument1`

`Argument 2: argument2`

Du kan deretter bruke disse argumentene til å utføre forskjellige handlinger i programmet ditt, for eksempel å sette verdier eller kjøre spesifikke funksjoner basert på brukerens input.

## Deep Dive

Det finnes også andre måter å lese kommandolinjeargumenter på, inkludert å bruke `Scanner`-klassen eller å lage egendefinerte flagg og alternativer. Det er viktig å huske at når du leser argumenter, vil de bli registrert som Strings selv om de kan være tall eller andre typer data. Du må derfor konvertere dem til ønsket type før du bruker dem i koden din.

En annen viktig ting å huske på er at rekkefølgen på argumentene som blir skrevet inn vil påvirke outputen din. Så sørg for å håndtere dem på en måte som passer best for ditt program.

## Se også

- [Java dokumentasjon om kommandolinjeargumenter](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java Scanner-dokumentasjon](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)
- [Java argument-parser bibliotek](https://github.com/toptensoftware/argparser)