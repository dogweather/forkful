---
title:                "C: Konvertere en streng til små bokstaver"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er ofte nødvendig å konvertere strenger til små bokstaver i programmering, spesielt hvis vi jobber med brukerinput eller sammenligner tekst. Konvertering til små bokstaver kan også gjøre det enklere å utføre søk og sammenligninger, og kan bidra til å unngå feil i koden.

## Slik gjør du det
Det finnes flere måter å konvertere en streng til små bokstaver i C-programmering. En vanlig metode er å bruke standardbibliotekfunksjonen `tolower()`. Denne funksjonen tar inn et tegn (char) som argument og returnerer tilsvarende små bokstav hvis det er et stort tegn (stor bokstav) eller samme tegn hvis det allerede er en små bokstav. Se eksempel under:

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
  char tekst[] = "HEI PÅ DEG!";
  for(int i = 0; tekst[i]; i++)
  {
    putchar(tolower(tekst[i]));
  }
  return 0;
}
```
**Output:**
hei på deg!

En annen måte å konvertere en streng til små bokstaver er ved å bruke ASCII-tabellen. ASCII-verdien til store bokstaver ligger mellom 65 og 90, mens ASCII-verdien for tilsvarende små bokstaver ligger mellom 97 og 122. Dermed kan vi bruke et enkelt regnestykke for å konvertere store bokstaver til små bokstaver. Se eksempel under:

```C
#include <stdio.h>

int main()
{
  char tekst[] = "HEI PÅ DEG!";
  for(int i = 0; tekst[i]; i++)
  {
    if(tekst[i] >= 65 && tekst[i] <= 90)
    {
      putchar(tekst[i] + 32); // ASCII-verdien + 32 gir tilsvarende små bokstav
    }
    else
    {
      putchar(tekst[i]);
    }
  }
  return 0;
}
```
**Output:**
hei på deg!

Det finnes også en rekke andre metoder for å konvertere en streng til små bokstaver, som å bruke standardbibliotekfunksjonen `strlwr()` eller å bruke `toupper()` funksjon for å konvertere alle bokstaver til store og deretter bruke ASCII-tabellen for å konvertere til små bokstaver. Det viktigste å huske på er å sjekke at strengen vi ønsker å konvertere ikke har noen spesialtegn, da disse kan gi feil resultat i konverteringen.

## Dypdykk
Å konvertere en streng til små bokstaver kan virke som en enkel oppgave, men det er faktisk ganske komplekst i C-programmering. I tillegg til å vite hvordan man konverterer bokstavene med riktig metode, er det også viktig å forstå hvordan datasystemet vårt håndterer bokstaver og hvordan ASCII-tabellen fungerer.

En vanlig feil som skjer når man konverterer strenger til små bokstaver i C er feil med tegnsettet. Hvis systemet vårt ikke støtter UTF-8 eller hvis vi ikke deklarerer det riktige tegnsettet i koden vår, kan det føre til at bokstavene blir konvertert feil og vi får uventet resultat. Derfor er det viktig å alltid sjekke og deklarere tegnsettet vårt før vi håndterer bokstaver eller tekst i koden vår.

## Se også
- [C - Standardbibliotek <ctype.h>](https://www.cplusplus.com/reference/cctype/)
- [ASCII-tabell](https://www.ascii-code.com/)