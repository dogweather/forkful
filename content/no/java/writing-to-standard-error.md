---
title:                "Skriving til standardfeil"
html_title:           "Java: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har programmert i Java, har du sannsynligvis sett uttrykket "standard error print" eller "std err". Dette refererer til å sende ut feil- eller loggmeldinger til terminalen eller konsollen under kjøring av et Java-program. Det kan være nyttig å sende ut informasjon til standard error i stedet for standard output i visse scenarier, for eksempel når du trenger å feilsøke eller logge kritisk informasjon.

## Hvordan

For å skrive til standard error i Java, kan du bruke print-metoder fra klassen `System.err`. Her er et eksempel på hvordan du kan bruke det i kombinasjon med et try-catch-blokk for å håndtere unntak:

```
try {
    // kode som kan kaste et unntak
} catch (Exception e) {
    System.err.println("En feil oppsto: " + e.getMessage());
}
```

I dette eksempelet vil eventuelle feilmeldinger bli sendt til standard error og skrevet ut på konsollen i stedet for standard output.

## Dypdykk

Det finnes også andre metoder for å skrive til standard error i Java, som for eksempel `System.err.write()`, `System.err.printf()` og `System.err.format()`. Disse gir deg ulike muligheter for å formatere og skrive ut tekst til standard error. En annen nyttig funksjon er `System.err.println()`, som automatisk legger til et linjeskift etter meldingen.

Det er også verdt å merke seg at du kan videreutvikle standard error ved å lage din egen underklasse av `PrintStream` og bruke den til å skrive til standard error i stedet for `System.err`. Dette kan være nyttig hvis du ønsker mer kontroll og fleksibilitet over hvordan meldingene dine blir formatert og håndtert.

## Se Også

- [Java - Writing to Standard Error](https://www.baeldung.com/java-writing-to-standard-error)
- [Writing to Standard Error in Java](https://www.geeksforgeeks.org/writing-to-standard-error-in-java/)
- [System.err documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)