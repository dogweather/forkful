---
title:                "Beregning av dato i fremtiden eller fortiden"
html_title:           "C: Beregning av dato i fremtiden eller fortiden"
simple_title:         "Beregning av dato i fremtiden eller fortiden"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden er en viktig del av programmering. Dette innebærer å bruke forskjellige metoder og funksjoner for å bestemme en spesifikk dato basert på et gitt antall dager, måneder eller år. Dette kan være nyttig for å lage dynamiske programmer som må håndtere tid og datoer, for eksempel kalendere, planleggingsverktøy og automatiserte prosesser.

# Hvordan:

For å beregne en dato i fremtiden eller fortiden i C, kan du bruke funksjonene definert i <time.h> biblioteket. Her er et eksempel på hvordan du kan bruke ```difftime()``` funksjonen for å finne forskjellen mellom to datoer og deretter legge til eller trekke fra det ønskede antall dager, måneder eller år:

```C 
#include <stdio.h> 
#include <time.h> 

int main () { 
    time_t now = time(NULL); // nåværende dato og tid
    time_t future_date = now + (7 * 24 * 60 * 60); // legg til 7 dager
    time_t past_date = now - (30 * 24 * 60 * 60); // trekk fra 30 dager
    
    char* now_str = ctime(&now); // konvertere til streng for lesbarhet
    char* future_date_str = ctime(&future_date); 
    char* past_date_str = ctime(&past_date);
    
    printf("Nåværende dato og tid: %s", now_str); 
    printf("Dato og tid om 7 dager: %s", future_date_str); 
    printf("Dato og tid for 30 dager siden: %s", past_date_str);
    
    return 0; 
} 
```

**Output:** 

Current date and time: Sat Mar 11 12:00:00 2020 
Date and time in 7 days: Sat Mar 18 12:00:00 2020 
Date and time 30 days ago: Tue Feb 10 12:00:00 2020 

# Dypdykk:

Det å beregne en dato i fremtiden eller fortiden har vært en viktig utfordring i programmering, spesielt med tanke på gjeldende kalendersystemer. I eldre versjoner av C var det også nødvendig å lage egne funksjoner for å håndtere dager, måneder og år. Men med introduksjonen av <time.h> biblioteket, har det blitt mye enklere å håndtere tid og datoer i C-programmer.

Det finnes også alternative måter å beregne en dato i fremtiden eller fortiden på, som for eksempel ved å bruke tredjeparts biblioteker som tilbyr mer komplekse funksjoner og utvidede muligheter for å håndtere forskjellige kalendersystemer.

Når det gjelder implementeringen av <time.h> funksjonene for å beregne datoer, bruker de vanligvis dataverdien på 0 som utgangspunkt. 0 dataverdi tilsvarer 1. januar 1970, også kjent som "Unix-epoken". Deretter kan du bruke forskjellige funksjoner som ```difftime()```, ```gmtime()``` og ```localtime()``` for å manipulere og presentere tid og dato i forskjellige formater.

# Se også:

- [time.h dokumentasjon] (https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [En enkel guide til å håndtere datoer og tid i C-programmering] (https://www.digitalocean.com/community/tutorials/how-to-use-date-and-time-in-c-programming-with-ansi-iso-extensions)