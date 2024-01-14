---
title:                "C: Skrive en tekstfil"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive en tekstfil ved hjelp av C-programmering kan være en utfordrende, men nyttig oppgave. Det gir deg muligheten til å programmere en fil som inneholder tekst som kan leses og behandles av datamaskinen. Dette kan være nyttig for å lagre data, lage rapporter, og mye mer. Det er også en viktig ferdighet å ha som en C-programmerer.

## Hvordan

For å skrive en tekstfil i C, må du først åpne en filstrøm ved hjelp av fopen() -funksjonen. Du må også bestemme hva slags modus du ønsker å åpne filen i, enten for lesing, skriving eller begge deler. Her er et eksempel på hvordan du kan åpne en fil for skriving:

```C
FILE *fp;
fp = fopen("minfil.txt", "w");
```

Nå som filstrømmen er åpnet, kan du skrive til filen ved hjelp av fprintf() -funksjonen. Denne funksjonen lar deg skrive forskjellige typer data til filen, for eksempel tekststrenger, tall og variabler. Her er et eksempel på hvordan du kan skrive til filen:

```C
fprintf(fp, "Dette er en tekst som blir skrevet til filen. \n");
```

Til slutt må du huske å lukke filstrømmen ved hjelp av fclose() -funksjonen når du er ferdig med å skrive til filen. Dette sikrer at alle dataene blir lagret riktig og at filen blir frigjort fra minnet. Her er et eksempel på hvordan du kan lukke filstrømmen:

```C
fclose(fp);
```

## Dykk dypere

Å skrive en tekstfil innebærer mer enn bare å åpne, skrive og lukke filen. Det finnes forskjellige moduser du kan åpne filen i, avhengig av hva du ønsker å gjøre med den. For eksempel kan du åpne en fil for lesing ved hjelp av "r"-modus, slik at du kan lese eksisterende tekst fra filen. I tillegg kan du også bruke "a"-modus for å legge til tekst i slutten av filen, i stedet for å skrive over eksisterende data.

I tillegg til å skrive til filen, er det også mulig å lese fra en tekstfil ved hjelp av fgets() -funksjonen. Dette lar deg lese en linje av tekst fra filen og lagre den i en variabel. Du kan også bruke fscanf() -funksjonen for å lese spesifikke typer data fra filen, som for eksempel tall.

## Se også

- [C-programmering: En begynnerguide](https://www.ntnu.no/wiki/kalkulus/cprog/)
- [C-filbehandling](https://www.studytonight.com/c/file-handling-in-c)
- [Mer om fopen() -funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)