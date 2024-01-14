---
title:                "C: Søking og bytting av tekst"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave i programmeringsverdenen. Dette kan være nyttig for å gjøre større endringer i en kodebase eller for å fikse mindre feil. Uansett hva årsaken er, er det viktig å forstå hvordan man gjør dette riktig for å unngå feil og ineffektivitet.

## Hvordan gjøre det

I C-programmering er det flere måter å søke og erstatte tekst på. En enkel måte er å bruke en innebygd funksjon som `str_replace()`. Denne funksjonen tar inn tre parametere: strengen du vil søke i, teksten du vil erstatte og teksten du vil erstatte den med. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char tekst[] = "Dette er en tekst.";
    char ny_tekst[] = str_replace(tekst, "en", "to");
    printf("%s\n", ny_tekst);
    return 0;
}
```

Dette eksemplet vil erstatte alle forekomster av "en" med "to" og skrive ut den nye teksten: "Dette er to tekst." Du kan også bruke `str_replace()` til å erstatte et tegn med et annet tegn, for eksempel:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char tekst[] = "Hello World!";
    char ny_tekst[] = str_replace(tekst, "!", "?");
    printf("%s\n", ny_tekst);
    return 0;
}
```

Denne koden vil erstatte utropstegnet med et spørsmålstegn og skrive ut den nye teksten: "Hello World?"

## Dypdykk

Selv om `str_replace()` er en enkel og effektiv måte å søke og erstatte tekst på, er det viktig å være oppmerksom på noen potensielle problemer. For det første, hvis du ønsker å erstatte en stor del av teksten din, kan denne funksjonen være svært ineffektiv fordi den bare erstatter ett tegn om gangen. Det er også verdt å merke seg at `str_replace()` er casesensitiv, så det vil ikke erstatte "Hello" med "hello" i eksemplene ovenfor.

Hvis du ønsker mer kontroll over søking og erstatting av tekst, kan du også bruke `strstr()` og `strtok()` funksjoner i C. Disse funksjonene lar deg søke etter en spesifikk del av teksten og deretter erstatte den med en annen tekst. De kan også brukes til å erstatte alle forekomster av en streng i teksten.

## Se også

- [C-programmering grunnleggende](https://www.ntnu.no/wiki/display/itsk/C+programmering+grunnleggende)
- [Offisiell dokumentasjon for str_replace-funksjonen](https://www.tutorialspoint.com/c_standard_library/c_function_str_replace.htm)
- [Eksempler på strstr og strtok-funksjoner](https://www.programiz.com/c-programming/library-function/string.h/strstr)