---
title:                "Java: Skriver til standardfeil"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive ut feilmeldinger til standard error er en viktig del av programmering, spesielt når det kommer til feilsøking og debugging. Ved å sende feilmeldinger til standard error i stedet for standard output, kan du enkelt skille dem fra vanlig utdata og få en mer oversiktlig og nøyaktig oversikt over potensielle feil i koden din.

## Slik gjør du det

For å skrive til standard error i Java, kan du bruke metoden `System.err.println()` som tar inn en `String`-verdi som parameter. Dette vil skrive ut teksten til standard error og legge til et linjeskift på slutten. La oss se på et eksempel:

```Java
public class StandardErrorExample {
    public static void main(String[] args) {
        System.err.println("Dette er en feilmelding");
    }
}
```

Når vi kjører dette programmet, vil vi få følgende utdata i konsollen:
```
Dette er en feilmelding
```

Som du ser, er det ikke forskjell på utseendet av utdata fra `System.err.println()` og vanlig `System.out.println()`, men ved å bruke sistnevnte vil det bli lettere å fange opp og håndtere eventuelle feilmeldinger.

## Dykk dypere

En liten ekstra detalj å merke seg er at `System.err` og `System.out` er to forskjellige `PrintStream`-objekter, og de styrer henholdsvis output til standard error og standard output i Java. Så ved å kalle `println()`-metoden på `System.err`, skriver du faktisk til `System.err`-objektet.

Det er også verdt å nevne at det finnes flere metoder i `PrintStream`-klassen som kan brukes for å skrive til standard error. For eksempel kan du bruke `System.err.write()` for å skrive en enkelt byte til standard error, eller `System.err.printf()` for å formatere utdata på en spesifikk måte.

## Se også

- Java API-dokumentasjon for `System`-klassen og `PrintStream`-klassen: [https://docs.oracle.com/javase/8/docs/api/][1]
- En guide til Java-feilsøking: [https://www.tutorialspoint.com/debugging-in-java][2]

[1]: https://docs.oracle.com/javase/8/docs/api/
[2]: https://www.tutorialspoint.com/debugging-in-java