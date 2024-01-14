---
title:    "C: Å skrive en tekstfil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være en viktig del av programmering, spesielt når det kommer til å lagre data og informasjon. Det kan være nyttig hvis du ønsker å lagre data som brukeren har lagt inn, eller hvis du trenger å generere en rapport med informasjon fra programmet ditt.

## Hvordan gjøre det

Å lage en tekstfil i C er ganske enkelt. Først må du åpne en fil ved hjelp av `fopen` -funksjonen og angi hvilken fil du vil åpne, og modusen du vil bruke (for eksempel lesing, skriving eller begge deler). Deretter kan du bruke `fputs` for å skrive teksten du ønsker å lagre i filen. Til slutt, må du huske å lukke filen med `fclose` for å sikre at dataene blir lagret riktig.

Her er et eksempel på hvordan du kan skrive teksten "Hei verden!" til en fil og deretter lese den og skrive den ut på skjermen:

```C
#include <stdio.h>

int main()
{
    // Åpne filen for skriving
    FILE *fil = fopen("tekstfil.txt", "w");

    // Skriv "Hei verden!" til filen
    fputs("Hei verden!", fil);

    // Lukk filen
    fclose(fil);

    // Åpne filen for lesing
    fil = fopen("tekstfil.txt", "r");

    // Les teksten fra filen og skriv den ut
    char tekst[50];
    fgets(tekst, 50, fil);
    printf("%s", tekst);

    // Lukk filen igjen
    fclose(fil);

    return 0;
}
```

Dette vil gi følgende utskrift:

```
Hei verden!
```

## Deep Dive

Når du skriver en tekstfil i C, må du være oppmerksom på noen ting. For det første må du sørge for at filen du åpner faktisk eksisterer, ellers vil `fopen` returnere `NULL` og du vil ikke kunne skrive til den. Du bør også alltid kontrollere at filen lukkes riktig ved å bruke `fclose`. Hvis du ikke gjør det, kan dataene dine gå tapt eller filen kan bli skadet.

En annen viktig ting å huske på er at når du skriver til en fil, vil innholdet fra tidligere tekst bli overskrevet med den nye teksten. Hvis du vil legge til tekst i slutten av en eksisterende fil, bør du bruke `fseek` til å sette markøren til slutten av filen før du skriver ny tekst.

## Se også

* ["Åpne og lese filer i C" av Programiz](https://www.programiz.com/c-programming/c-file-input-output)
* ["C-filer og strømmer" av GeeksforGeeks](https://www.geeksforgeeks.org/c-files-and-streams/)
* ["C-filer" av Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)