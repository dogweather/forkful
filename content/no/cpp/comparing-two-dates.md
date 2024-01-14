---
title:    "C++: Sammenligning av to datoer"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Sammenligning av to data kan være en viktig del av programmeringsoppgaver, spesielt når man jobber med datoer og tidspunkter. Dette kan være nyttig for å finne ut om en hendelse har skjedd før eller etter en annen hendelse, eller for å sortere og filtrere data basert på datoer. I denne bloggposten skal vi se på hvordan man kan sammenligne to datoer ved hjelp av C++.

## Hvordan
Sammenligning av to datoer kan gjøres ved hjelp av noen enkle operasjoner i C++. Først må vi opprette to variabler som inneholder datoene vi ønsker å sammenligne. Dette kan gjøres på flere måter, men et enkelt eksempel kan være å bruke struct-typen `tm` fra `<ctime>` biblioteket. Denne typen lar deg definere datoer ved hjelp av år, måned, dag og klokkeslett.

Vi bruker deretter funksjonen `mktime()` til å konvertere disse verdiene til en `time_t`-verdi, som er et tidsstempel som representerer antall sekunder siden 1. januar 1970. Ved å konvertere datoene til `time_t`-verdier, kan vi enkelt sammenligne dem ved hjelp av enkle matematiske operasjoner.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Oppretter to variabler som inneholder datoene vi vil sammenligne
    tm date1 = {2021, 9, 25, 0, 0, 0}; 
    tm date2 = {2021, 9, 30, 0, 0, 0};
    
    // Konverterer datoene til time_t-verdier
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);
    
    // Sammenligner datoene og printer ut resultatet
    if (difftime(time1, time2) < 0) {
      std::cout << "Date 1 er tidligere enn date 2" << std::endl;
    } else if (difftime(time1, time2) == 0) {
      std::cout << "Date 1 og date 2 er like" << std::endl;
    } else {
      std::cout << "Date 1 er senere enn date 2" << std::endl;
    }
    
    return 0;
}
```

Dette eksempelet bruker funksjonen `difftime()` til å sammenligne to `time_t`-verdier og returnere differansen mellom dem i sekunder. Om differansen er mindre enn 0, betyr det at `time1` er tidligere enn `time2`, om den er 0, betyr det at datoene er like, og om den er større enn 0, betyr det at `time1` er senere enn `time2`.

I eksempelet over brukte vi kun datoer, men samme metode kan også brukes for å sammenligne klokkeslett.

## Dykk dypere
Det finnes flere metoder for å sammenligne datoer i C++, som for eksempel å bruke `<chrono>` biblioteket eller å bruke funksjoner for å konvertere datoer til ulike formater som kan sammenlignes. Det er også viktig å være klar over hvordan datoer blir representert i ulike tidssoner og hvordan dette kan påvirke sammenligningen. Det er alltid lurt å grundig teste sammenligningsmetoden din for å sikre nøyaktighet.

## Se også
- [C++ referanse - <ctime> biblioteket](https://www.cplusplus.com/reference/ctime/)
- [C++ referanse - <chrono> biblioteket](https://www.cplusplus.com/reference/chrono/)
- [Stack Overflow - How to compare two dates in C++](https://stackoverflow.com/questions/14457370/how-to-compare-two-dates-in-c)