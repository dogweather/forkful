---
title:                "Ekstrahering av substringer"
html_title:           "Java: Ekstrahering av substringer"
simple_title:         "Ekstrahering av substringer"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle man bry seg med å trekke ut substrings? Vel, substrings er en viktig konsept innenfor strengmanipulering, og å kunne trekke ut deler av en streng kan være nyttig for å bearbeide data eller filtrere informasjon.

## Slik Gjør Du Det

Koden nedenfor viser hvordan man kan trekke ut en substring fra en streng ved å bruke `substring()` metoden i Java:

```Java
String tekst = "Dette er en test av substrings";
String substring = tekst.substring(13, 18); // Trekker ut substring fra indeks 13 til 18
System.out.println(substring); // Output: "en test"
```

For å forstå koden bedre, la oss bryte den ned:

- Først definerer vi en streng, `tekst`, som vi ønsker å trekke ut en substring fra.
- Deretter bruker vi `substring()` metoden på `tekst`-strengen. Denne metoden tar inn to parametere, `startIndeks` og `sluttIndeks`, som indikerer hvor i strengen substringen skal starte og slutte.
- Til slutt skriver vi ut resultatet ved å bruke `println()` metoden, som sørger for at resultatet blir printet til konsollen.

Merk at indeksen starter på 0 i Java, så for å trekke ut ordet "en", som er fra indeks 13 til 15 i `tekst`-strengen, må vi bruke `substring(13, 15)`.

## Dypdykk

For å få en bedre forståelse av substring-konseptet, la oss se på hvordan metoden `substring()` fungerer under panseret. 

`Substring()`-metoden er en del av `String`-klassen i Java, og den tar inn to parametere, `startIndeks` og `sluttIndeks`. Når metoden blir kalt, blir en ny streng opprettet ved å kopiere deler av den opprinnelige strengen mellom indeksene som blir spesifisert. Den nye strengen blir returnert som et resultat. 

Det er viktig å merke seg at den opprinnelige strengen forblir uendret - metoden returnerer kun en kopi av delen av strengen du har bedt om å trekke ut. Dette betyr at kun en ny instans av en streng blir opprettet, noe som er mer effektivt enn å lage en hel ny streng ved å manipulere den opprinnelige.

## Se Også

For mer informasjon om strenger og substring-manipulering i Java, sjekk ut disse ressursene:

- [Java Docs: String Class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [W3Schools: Java Substring() Method](https://www.w3schools.com/java/ref_string_substring.asp)