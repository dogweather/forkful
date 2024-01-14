---
title:    "C: Generering av tilfeldige tall"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hva som skjer når du trykker på en knapp og en ny tilfeldig tall vises på skjermen din? Svaret er at det er takket være programmering av tilfeldige tall! Dette er en viktig del av mange programmer og spill, og i denne bloggposten vil vi utforske hvordan det fungerer.

## Slik gjør du det

For å starte, må du inkludere biblioteket \<stdlib.h\> i koden din. Dette biblioteket inkluderer funksjoner som lar deg generere tilfeldige tall i C-programmering. La oss se på et enkelt eksempel på hvordan du kan bruke disse funksjonene:

```C
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    // Bruk funksjonen rand() for å generere et tilfeldig tall
    int random_number = rand();

    // Vis tilfeldig tall på skjermen
    printf("Tilfeldig tall: %d", random_number);

    return 0;
}

```
Kjører denne koden vil gi deg et tilfeldig tall hver gang du kjører programmet. Men hvis du alltid får det samme tallet, kan du legge til en linje med koden ``` srand(time(NULL));``` for å sikre at hvert tilfeldig tall er unikt basert på tiden når koden kjøres.

Hvis du trenger å generere et tilfeldig tall innenfor et gitt område, kan du bruke funksjonen rand() med modulus-operatøren. For eksempel, hvis du vil ha et tilfeldig tall mellom 1 og 10, kan koden din se slik ut:

```C
int random_number = 1 + rand() % 10;
```

Hvis du ønsker å generere flere tilfeldige tall, kan du bruke en løkke i koden din. Dette vil tillate deg å generere et gitt antall tilfeldige tall og lagre dem i et array, for eksempel. Her er et eksempel på en for-løkke som genererer 10 tilfeldige tall og lagrer dem i et array:

```C
int random_numbers[10];

for (int i = 0; i < 10; i++) {
    random_numbers[i] = rand();
}
```

## Dypdykk

Mens tilfeldige tall kan virke helt tilfeldig for oss, er det faktisk et system bak genereringen av dem. Et tilfeldig tall i programmering er ikke helt tilfeldig, men det er heller ikke et forutsigbart mønster. Det er basert på en matematisk formel, kalt en pseudorandom generator (PRNG), som produserer tall som ser tilfeldige ut, men som egentlig følger en bestemt algoritme.

I C-programmering bruker vi vanligvis en PRNG kalt "linear congruential generator". Denne metoden genererer tilfeldige tall ved å multiplisere det forrige tilfeldige tallet med en konstant og deretter legge til en annen konstant. Dette forrige tallet kalles "seed" og det er vanligvis en verdi basert på systemklokken. Dette sikrer at hver gang koden kjøres, vil du få et unikt utgangspunkt for tilfeldige tall.

Det er også viktig å merke seg at PRNGs kan gjenta seg selv etter en viss tid. Dette betyr at hvis koden din kjører lenge nok, kan den potensielt gjenta de samme tilfeldige tallene. Derfor må du være forsiktig med hvor du bruker tilfeldige tall, og vurdere å bruke en annen metode for å generere virkelig tilfeldige tall, som for eksempel å bruke sensorinput til å påvirke PRNG-algoritmen.

## Se også
- [C-programmering på norsk](https://www.ntnu.no/wiki/display/gamecc/C-programmering+p%C3%A5+norsk)
- [Offisiell C-programmeringsbok](https://www.norli.no/kunnskapsformidling/ikke-kategoriserte-artikler/c-programmering)
- [Tilfeldige tall generert av en datamaskin](http://www.mcs.sds