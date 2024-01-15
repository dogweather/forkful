---
title:                "Skriving til standard feil"
html_title:           "C: Skriving til standard feil"
simple_title:         "Skriving til standard feil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til å skrive til standardfeil i C-programmering. Det er en nyttig måte å få informasjon om feil og problemer i programmet ditt, som kan hjelpe deg med å finne og rette feil. Det kan også være nyttig når du tester og debugger koden din.

## Hvordan gjøre det

For å skrive til standardfeil i C, kan du bruke funksjonen "fprintf". Dette er en inkludert funksjon i C som lar deg skrive til en filstrøm, inkludert standardfeil. Du må inkludere headerfilen "stdio.h" for å bruke denne funksjonen.

For å bruke "fprintf" funksjonen, må du først åpne filstrømmen ved hjelp av "stderr" som argument, som er standardfeilstrømmen. Deretter kan du bruke vanlig formatering som i "printf" funksjonen for å skrive ut dataene du ønsker å feilstrømmen. For eksempel:

```C
fprintf(stderr, "Dette er en feilmelding: %s\n", "Feil ved åpning av fil.");
```

Dette vil skrive ut en feilmelding til standardfeilstrømmen og inkludere den angitte teksten i formateringen, som i dette tilfellet er "Feil ved åpning av fil.".

Du kan også bruke "stderr" direkte i stedet for "fprintf" funksjonen, men dette begrenser deg til å kun skrive ut en streng uten formatering. For eksempel:

```C
fprintf(stderr, "Dette er en feilmelding.");
```

## Dypdykk

Når du bruker "fprintf" funksjonen for å skrive til standardfeil, er det viktig å huske på å inkludere "\n" for å få en nylinje etter meldingen. Dette vil gjøre feilmeldingene dine lettere å lese og skille fra hverandre.

Det kan også være nyttig å inkludere feilkode i feilmeldingene dine for å identifisere og håndtere forskjellige feil. Du kan gjøre dette ved å bruke "errno" variabelen, som inneholder feilkoden for siste funksjonskall.

En viktig ting å huske på er at når du skriver til standardfeil, vil meldingen vises på skjermen selv om programmet ditt kjøres som en daemon eller i bakgrunnen. Dette kan være forvirrende for brukeren og kan føre til sikkerhetsrisikoer, så det er viktig å håndtere feilmeldingene dine på en sikker og effektiv måte.

## Se også

- [The Standard Error Stream](https://www.linuxjournal.com/article/6589)
- [How to Redirect Standard Error in C](https://www.geeksforgeeks.org/how-to-redirect-stderr-to-a-file-in-c/)
- [Error Handling in C](https://www.tutorialspoint.com/cprogramming/c_error_handling.htm)