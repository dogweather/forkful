---
title:    "C: Å lese en tekstfil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese og håndtere tekstfiler er en viktig del av programmering. Det er ofte nødvendig å kunne lese informasjon fra en tekstfil, enten det er for å behandle data eller for å vise resultatene til brukeren. I denne artikkelen vil vi forklare hvordan du kan lese en tekstfil i C-programmering, slik at du kan bruke denne kunnskapen i dine egne prosjekter.

## Hvordan

For å lese en tekstfil i C-programmering, trenger du først å åpne filen ved hjelp av fopen-funksjonen. Du må også spesifisere hvilken fil du ønsker å lese og hvilken modus du skal lese den i. Det finnes forskjellige moduser du kan åpne en fil i, som for eksempel "r" for å lese, "w" for å skrive og "a" for å legge til tekst i slutten av filen.

```C
FILE *file = fopen("tekstfil.txt", "r");
```

Når filen er åpnet, kan du lese linje for linje ved hjelp av fgets-funksjonen og lagre innholdet i en variabel. Du må også passe på å lukke filen ved hjelp av fclose-funksjonen når du er ferdig med å lese den.

```C
char line[100];
while (fgets(line, 100, file) != NULL) {
    // Gjør noe med linjen
}
fclose(file);
```

Det er viktig å huske på at lesing av en tekstfil kan være feilfølsomt, spesielt hvis filen inneholder forskjellige tegnsett som for eksempel æ, ø og å. Det er derfor viktig å konvertere teksten til riktig tegnkoding ved hjelp av f.eks. "setlocale" -funksjonen.

## Dykk dypere

Å lese en tekstfil i C kan også gjøres ved hjelp av "fscanf" -funksjonen, som lar deg lese og tolke hver linje direkte. Dette kan være nyttig hvis filen inneholder strukturerte data som f.eks. tall eller strenger.

En annen viktig funksjon i C for å håndtere tekstfiler er "fprintf" -funksjonen, som lar deg skrive tekst og variabler til en fil. Dette kan være nyttig for å lagre data eller resultater fra programmet ditt.

Husk også å sjekke for eventuelle feil underveis, og håndtere dem på en hensiktsmessig måte, for eksempel ved å gi en feilmelding eller lukke programmet.

## Se også

- [C-funksjoner for filbehandling](https://www.tutorialspoint.com/c_standard_library)
- [Eksempler på å lese og skrive til en tekstfil i C](https://www.programiz.com/c-programming/c-file-input-output)
- [Hvordan lese data fra CSV-filer i C](https://www.geeksforgeeks.org/csv-files-in-c-syntax-working-with-csv-files/)