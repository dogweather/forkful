---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Arduino: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmerere. Dette innebærer å bruke en algoritme for å finne ut når en bestemt dato vil være basert på en gitt startdato og et gitt antall dager. Dette kan være nyttig for en rekke applikasjoner, som for eksempel å lage en kalender eller en tidsstyringsprogramvare.

# Hvordan:

En enkel måte å beregne en dato i fremtiden eller fortiden i Arduino er å bruke datofunksjoner innebygd i språket. Her er et eksempel på å finne ut datoen 30 dager fra nå:

```arduino
#include <Time.h>
// Definer startdato
int dag = 5;
int måned = 9;
int år = 2020;

// Legg til 30 dager
int dagerTillegg = 30;

// Bruk setTime funksjonen for å sette startdatoen
setTime(0,0,0,dag,måned,år);

// Bruk time_t funksjonen til å beregne datoen i fremtiden
time_t futureDate = now() + (dagerTillegg * SECS_PER_DAY);

// Bruk nåværende tid funksjonen til å få dato i riktig format
tmElements_t futureElements;
breakTime(futureDate, futureElements);

// Skriv ut datoen
Serial.print(futureElements.Day); Serial.print("/"); Serial.print(futureElements.Month); Serial.print("/"); Serial.print(futureElements.Year);
```

Dette vil resultere i følgende utskrift:

```
4/10/2020
```

# Dypdykk:

Beregning av datoer har vært en viktig del av programmering i lang tid. Før datamaskiner ble vanlige, måtte folk regne ut datoer manuelt. Dette førte til utvikling av ulike algoritmer for å forenkle prosessen. En alternativ måte å beregne datoer på i Arduino er å bruke en tredjeparts bibliotek, som TimeLib eller DateTime. Disse kan tilby flere funksjoner og muligheter for beregning av datoer.

En ting å merke seg er at Arduino ikke har innebygde funksjoner for å håndtere skuddår. Dette kan føre til feil i datoene dersom de ikke blir inkludert eller håndtert manuelt.

# Se også:

- [TimeLib bibliotek](https://www.arduino.cc/en/Reference/Time)
- [DateTime bibliotek](https://www.arduino.cc/en/Reference/DateTime)