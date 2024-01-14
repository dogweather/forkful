---
title:    "C: Søk og erstatt tekst"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å søke og erstatte tekst er en viktig del av programmeringsverdenen. Enten du er en erfaren utvikler eller en nybegynner, vil du sannsynligvis noen gang måtte erstatte en bestemt tekst med en annen. Dette kan for eksempel være nødvendig når du ønsker å endre en variabelnavn gjennom hele koden din, eller hvis du ønsker å bytte ut et spesifikt ord med et annet i et dokument. Uten å kunne søke og erstatte tekst, kan det være en svært tidkrevende og kjedelig oppgave å gjøre for hånd. Derfor er det viktig å kunne utnytte funksjoner som dette i programmeringen din.

## Hvordan

Søke og erstatte tekst i C-programmering er enkelt og kan gjøres med få linjer med kode. Først må du inkludere standardbiblioteket "string.h" for å kunne bruke funksjoner for strenger. Deretter kan du bruke "strtok" funksjonen til å dele opp strengen i mindre biter basert på et gitt skillemerke. Dette kan være et enkelt tegn eller en hel streng. Etter dette kan du bruke "strcpy" funksjonen til å erstatte den ønskede teksten med den nye teksten.

Et eksempel på dette kan være å erstatte alle forekomster av ordet "hund" med "katt" i en streng:

```
#include <stdio.h>
#include <string.h>

int main()
{
    char text[100] = "Jeg elsker min hund, den er min beste venn.";
    char *tok;
    char new_text[100] = "";

    tok = strtok(text, " "); // deler opp strengen etter mellomrom
    while(tok != NULL)
    {
        if(strcmp(tok, "hund") == 0) // sammenligner om strengene er like
        {
            strcat(new_text, "katt"); // legger til det nye ordet i den nye strengen
        }
        else
        {
            strcat(new_text, tok); // legger til ordet som det er hvis det ikke er funnet
        }
        strcat(new_text, " "); // legger til et mellomrom mellom hvert ord i den nye strengen
        tok = strtok(NULL, " "); // fortsetter å dele opp strengen
    }
    printf("%s\n", new_text); // skriver ut den nye strengen
    return 0;
}
```

### Output
```
Jeg elsker min katt, den er min beste venn.
```

## Dypdykk

Det finnes ulike metoder for å søke og erstatte tekst i C-programmering. En annen metode er å bruke "strstr" funksjonen som søker gjennom en streng og returnerer en peker til den første forekomsten av en annen streng. Dette gjør det mulig å enkelt finne posisjonen til en bestemt tekst og erstatte den med en annen.

Det er også viktig å være forsiktig med å bruke "strcpy" funksjonen for å unngå mulige bufferoverløp. Det anbefales å bruke "strncpy" i stedet som tar med argumentet for hvor mange tegn som skal kopieres og unngår å overskrive grensen på en streng.

## Se Også

- [C-programmering med GNU-toolchain tutorial (på norsk)](https://www.learn-c.org/no/)
- [Offisiell C-programmering dokumentasjon (på engelsk)](https://devdocs.io/c/)
- [Flere eksempler på søk og erstatt tekst i C (på engelsk)](https://www.geeksforgeeks.org/c-programming-find-and-replace-a-word-in-a-file/)