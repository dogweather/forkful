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

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge prosjekter eller hendelser. Denne funksjonen kan også være nyttig for å spore tidsbaserte hendelser i et Arduino-prosjekt, som for eksempel å aktivere et lys til en bestemt tid.

## Slik gjør du det

Du kan beregne en dato i fremtiden eller fortiden ved hjelp av funksjonene `day()` og `month()` i Arduino. Først må du definere en variabel for å representere dag og måned. Deretter kan du bruke `month()` til å beregne måneden i fremtiden eller fortiden, og `day()` til å beregne dagen. Se eksempelet nedenfor:

```
Arduino
int day; //definerer en variabel for dag
int month; //definerer en variabel for måned

//beregner en dato 100 dager frem i tid
day = day() + 100;
month = month();

//beregner en dato 1 måned tilbake i tid
day = day() - 1;
month = month() - 1;
```

Du kan også bruke `if`- og `else`-betingelser for å beregne en dato basert på brukerinput eller andre variabler i koden din.

## Dypdykk

Datoer beregnes vanligvis i forhold til en referansedato, som vanligvis er 1. januar. Arduino bruker også denne referansedatoen, men du kan også endre den ved å bruke funksjonen `setTime()`.

Det er også viktig å merke seg at Arduino følger den gregorianske kalenderen, så du må ta hensyn til skuddår når du beregner datoer.

## Se også

- [Arduino dokumentasjon om dato og tid](https://www.arduino.cc/reference/en/language/functions/time/)
- [Tutorial: Tid og dato i Arduino](https://learn.adafruit.com/arduino-tips-tricks-and-techniques/timing-issues)
- [Beregning av dato og tid i C++](https://en.cppreference.com/w/cpp/chrono/c/month)