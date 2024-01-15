---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger trenger vi å beregne en dato i fremtiden eller fortiden, for eksempel for å planlegge en viktig hendelse eller for å sjekke noe i fortiden. Med C-programmeringsspråket kan vi enkelt lage et program som kan hjelpe oss med å beregne slike datoer.

## Hvordan

Det første vi må gjøre er å inkludere biblioteket `time.h` i programmet vårt. Dette biblioteket inneholder funksjoner for å håndtere tid og datoer. Deretter må vi opprette variabler for å lagre dag, måned og årstall for den ønskede datoen.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Oppretter variabler for dag, måned og årstall
    int dag, måned, år;
}
```

Vi kan da bruke funksjonen `scanf` til å be brukeren om å skrive inn disse verdiene, eller vi kan hardkode dem i programmet vårt. Deretter må vi konvertere disse verdiene til et `struct tm`-objekt ved hjelp av funksjonen `mktime`.

```C
struct tm dato; // Oppretter et struct tm-objekt
dato.tm_year = år - 1900; // Lager om til riktig format
dato.tm_mon = måned - 1;
dato.tm_mday = dag;
dato.tm_hour = 0;
dato.tm_min = 0;
dato.tm_sec = 0;

time_t t = mktime(&dato); // Konverterer til time_t-objekt
```

Nå kan vi bruke funksjonen `gmtime` til å få dag, måned og år for denne datoen, og videre bruke funksjonen `strftime` for å formatere denne informasjonen slik vi ønsker.

```C
struct tm *resultat = gmtime(&t); // Lager et struct tm-objekt med dato
char buffer[80];

strftime(buffer, 80, "Datoen er %d.%m.%Y", resultat); // Formatterer informasjonen
printf("%s\n", buffer); // Skriver ut resultatet
```

For å beregne en dato i fremtiden eller fortiden, kan vi bruke funksjonen `mktime` til å konvertere datoen til et time_t-objekt, legge til eller trekke ifra antall sekunder vi ønsker, og deretter bruke `gmtime` og `strftime` til å formatere informasjonen.

## Dypdykk

Når vi beregner en dato i fremtiden eller fortiden, må vi ta hensyn til skuddår og forskjellige antall dager i månedene. Dette er grunnen til at vi må konvertere datoen til et `struct tm`-objekt før vi konverterer det til et time_t-objekt. Dette biblioteket håndterer også skiftet til sommertid og vintertid.

## Se også

- [Offisiell C-dokumentasjon](https://www.iso-9899.info/wiki/The_Standard)
- [C-programmering på W3Schools](https://www.w3schools.in/c-tutorial/)
- [Mer om tid og dato i C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)