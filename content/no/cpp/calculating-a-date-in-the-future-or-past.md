---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C++: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være nyttig å kunne beregne en dato i fremtiden eller fortiden for å planlegge avtaler, organisere tidsfrister eller for skreddersydd rapportering.

## Hvordan
Det er enkelt å beregne en dato i fremtiden eller fortiden ved hjelp av C++ programvare. Følg disse trinnene:

### Steg 1: Inkluder tidsløsningen

For å utføre beregninger på datoer, må du inkludere tidsløsningsbiblioteket i C++. Dette kan gjøres ved å legge til følgende kode i toppen av filen din:
```
#include <chrono>
```

### Steg 2: Definer en variabel for den nåværende datoen

Du kan definere en variabel for den nåværende datoen ved hjelp av tidsløsningsklassen ```std::chrono::system_clock```. Dette kan gjøres ved å legge til følgende kode:
```
auto nå = std::chrono::system_clock::now();
```

### Steg 3: Bruk tidsenheten for å legge til eller trekke fra dager, uker, måneder eller år fra den nåværende datoen

Tidsløsningsklassen inkluderer ulike tidsenheter som kan brukes til å legge til eller trekke fra tid fra den nåværende datoen. Her er noen eksempler:
```
// legg til 10 dager til den nåværende datoen
nå += std::chrono::duration<int, std::ratio<86400>>(10); 
// trekke fra 2 uker fra den nåværende datoen
nå -= std::chrono::duration<int, std::ratio<604800>>(2); 
// legg til 5 måneder til den nåværende datoen
nå += std::chrono::duration<int, std::ratio<2629743>>(5); 
// trekke fra 1 år fra den nåværende datoen
nå -= std::chrono::duration<int, std::ratio<31556926>>(1); 
```

### Steg 4: Konverter datoen til ønsket format og skriv ut resultatet

Til slutt kan du konvertere den beregnede datoen til ønsket format og skrive ut resultatet. Det kan se noe slik ut:
```
// konverter datoen til en lesbar string og skriv ut
auto beregnet_dato = std::chrono::system_clock::to_time_t(nå);
std::cout << "Beregnet dato: " << std::put_time(std::localtime(&beregnet_dato), "%F") << std::endl;
```

## Deep Dive
Tidsløsningsbiblioteket i C++ bruker et 64-bit antall sekunder siden 1. januar 1970 som basis for tidsenheter. Det er viktig å ta hensyn til ulike tidsenhetskonverteringer for å unngå unøyaktigheter i de beregnede datoene.

## Se Også
- [C++ std::chrono bibliotek-dokumentasjon] (https://www.cplusplus.com/reference/chrono/) 
- [C++ tutorial om å beregne datoer] (https://www.cplusplus.com/forum/articles/12317/)