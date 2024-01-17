---
title:                "Sammenføyning av strenger"
html_title:           "C: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
 I C programmering, concatenation refers til å legge til to eller flere tekststrenger sammen for å danne en lengre streng. Dette kan være nyttig for å lage dynamiske utskriftssetninger eller bygge tekstbaserte brukergrensesnitt. Programmører bruker ofte denne funksjonaliteten for å effektivt håndtere tekstvariabler og lage mer avanserte programmer. 

## Slik gjør du:
Lag to strenger med tekst og bruk `strcat()` funksjonen for å kombinere dem. Her er et eksempel hvor vi legger til "Hei " til "verden" for å få setningen "Hei verden":

```
C char string1[20] = "Hei ";
char string2[10] = "verden";
strcat(string1, string2);
printf("%s", string1);
```

Output:
```
Hei verden
```

## Dypdykk:
Historisk sett var konkatenering av tekst gjort ved å bruke `sprintf()` funksjonen, men den nye standarden i C foretrekker `strcat()` funksjonen. En annen mulighet er å bruke `strcpy()` sammen med `strlen()` for å kopiere en streng og deretter finne lengden av den og legge til den andre. Implementasjonen av `strcat()` bruker en løkke og pekere for å håndtere konkatenering av to strenger. 

## Se også:
 For mer informasjon om bruk av `strcat()`, se [dokumentasjonen til C](https://www.tutorialspoint.com/c_standard_library/string_h.htm). For en dypere forståelse av strenger i C, se [dette nettstedet](https://www.programiz.com/c-programming/c-strings).