---
title:                "C: Å lese en tekstfil"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil i et C-program kan være en nyttig og viktig ferdighet for enhver programmerer. Det lar deg hente data fra en fil og bruke den i ditt program, noe som kan bidra til å gjøre det mer dynamisk og interaktivt.

## Hvordan

Vi kan lese en tekstfil ved hjelp av den innebygde funksjonen `fopen()` og `fscanf()`. Først må vi åpne filen ved å gi navnet og modusen (for å lese) som parametere til `fopen()`. Så kan vi bruke `fscanf()` til å lese dataen fra filen og lagre den i ønskede variabler. For eksempel:

```C
FILE *fil;
char navn[20];
int alder;

fil = fopen("brukerinfo.txt", "r");
fscanf(fil, "%s %d", &navn, &alder);
printf("Hei, %s. Du er %d år gammel.", navn, alder);
```

Dette vil åpne filen med navnet "brukerinfo.txt" og lese informasjonen fra den første linjen, som forventes å inneholde et navn og en alder, og skrive ut en personlig hilsen til terminalen.

For å unngå potensielle feil må vi også huske å lukke filen med `fclose()` etter at vi er ferdig med å lese fra den.

## Dypdykk

Det er viktig å merke seg at dataen som leses fra en tekstfil vil bli lagret som tekststrenger og må konverteres til riktig datatyper hvis de skal brukes som tall eller andre data. Dette kan gjøres ved hjelp av funksjoner som `atoi()` eller `atof()`.

I tillegg er det også viktig å sjekke om åpning av filen var vellykket ved å sjekke om `fopen()` returnerer en NULL-verdi. Hvis dette skjer, betyr det at filen ikke ble funnet eller at det var en annen feil under åpningen.

## Se Også

- [fopen() documentation](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [fscanf() documentation](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)