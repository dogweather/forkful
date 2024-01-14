---
title:                "C: Kombinering av strenger"
simple_title:         "Kombinering av strenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger er en nødvendig ferdighet for enhver som ønsker å lære programmering. Dette er fordi det er en vanlig oppgave som må utføres når man jobber med tekstbaserte data og programvare. Ved å forstå hvordan man kan konkatenerere strenger, vil du kunne manipulere tekst på en mer effektiv måte og få et bedre grep om grunnleggende programmeringskonsepter.

## Hvordan

For å kombinere to strenger i C, kan du bruke strcpy() funksjonen. Denne funksjonen kopierer en streng til en annen på en effektiv og sikker måte. Sjekk ut eksempelet under for å se hvordan det gjøres:
```C
#include <stdio.h>

int main() {
    char streng1[] = "Hei";
    char streng2[] = "verden!";
    char resultat[13];

    strcpy(resultat, streng1);
    strcat(resultat, streng2);

    printf("Kombinert streng: %s", resultat);
    return 0;
}
```
Output:
```
Kombinert streng: Hei verden!
```

## Dypdykk

Det finnes også andre måter å kombinere strenger på i C, som for eksempel ved hjelp av sprintf() funksjonen. Det er viktig å være oppmerksom på at når man kobinerer strenger, må man ha nok plass i minnet for å lagre den kombinerte strengen. Hvis du ikke sørger for dette, kan programmet ditt krasje eller gi uforventet output.

## Se også

- [C String Functions](https://www.programiz.com/c-programming/string-handling-functions)
- [Introduction to C Programming](https://www.learn-c.org/)