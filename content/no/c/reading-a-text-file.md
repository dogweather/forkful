---
title:                "C: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en grunnleggende og nødvendig del av å programmere i C. Ved å lese en tekstfil kan du få tilgang til ulike former for data som kan brukes i programmet ditt. Dette kan for eksempel være en liste over brukernavn og passord, eller en liste over produkter som skal vises i en nettbutikk. Uansett hva slags informasjon du trenger, kan du finne det i en tekstfil.

## Slik gjør du det

For å lese en tekstfil i C, må du følge disse enkle stegene:

1. Åpne tekstfilen: Her bruker du funksjonen `fopen()` for å åpne tekstfilen og få tilgang til den. Du må angi filnavnet og åpne den i lesmodus ved å bruke bokstaven `r` som parameter.
2. Sjekke om filen er åpen: Før du kan begynne å lese fra tekstfilen, må du sørge for at den faktisk er åpen. Dette kan du gjøre ved å bruke `if`-setningen og sjekke om `fopen()` returnerer en gyldig peker.
3. Les tekstfilen: Du kan bruke funksjonen `fgets()` til å lese linjer fra tekstfilen, en etter en. Husk å lukke filen når du er ferdig med å lese.

```C
#include <stdio.h>
int main()
{
    FILE *fptr;
    char tekst[100];

    // Åpne filen
    fptr = fopen("tekstfil.txt", "r");
    
    // Sjekke om filen er åpen
    if (fptr == NULL)
    {
        printf("Feil ved å åpne filen.");
        return 1;
    }
    
    // Les tekstfilen og skriv ut innholdet
    while (fgets(tekst, 100, fptr) != NULL)
    {
        printf("%s", tekst);
    }
    
    // Lukk filen
    fclose(fptr);
    return 0;
}
```

Output:

```bash
Dette er en tekstfil.
Den inneholder noen eksempelsetninger.
Du kan lese denne teksten med et enkelt C-program.
```

## Dykk dypere

Det finnes andre måter å lese en tekstfil på, for eksempel med funksjonene `fgetc()` og `fscanf()`. Disse har forskjellige bruksområder, og det kan være lurt å utforske dem nærmere for å finne ut hva som passer best til din situasjon. Det er også viktig å huske å håndtere eventuelle feil som kan oppstå ved å bruke disse funksjonene. Du kan lese mer om dette i C-programmeringens dokumentasjon eller andre online ressurser.

## Se også

- [C-programmeringens offisielle dokumentasjon](https://devdocs.io/c/)
- [Lesing og skriving av filer i C](https://www.programiz.com/c-programming/c-file-input-output) (på engelsk)
- [Grunnleggende om tekstbehandling i C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm) (på engelsk)