---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er rett og slett en måte for programmerere å fjerne uønsket informasjon fra en tekststreng. Dette kan være nyttig for å rense og strukturere data, for eksempel når man jobber med sensoravlesninger.

## Hvordan:
For å slette tegn som matcher et mønster i Arduino, kan man bruke funksjonen "replace()". Denne funksjonen tar inn tre parametere: teksten man vil søke gjennom, mønsteret man ønsker å slette og teksten man vil erstatte matchen med. 

```Arduino
replace("Det er en tekst", "er", "");
```

Dette vil gi følgende output:

```Arduino
"Dt en tekst"
```

Man kan også bruke denne funksjonen til å fjerne flere tegn ved å erstatte det man ønsker å slette med et lengre tomt tekststreng. For eksempel:

``` Arduino
replace("abc123def456", "123", "   ");
```

Dette vil gi følgende output:

```Arduino
"abcdef456"
```

## Dykk dypere:
Sletting av tegn som matcher et mønster har vært en vanlig teknikk i programmering i mange år. En alternativ måte å gjøre dette på er å bruke såkalte regulære uttrykk, som er et mer komplekst mønster-søke-verktøy. Men for enkle søk og slettinger, er "replace()" i Arduino en enkel og effektiv løsning.

## Se også:
- [Dokumentasjon om funksjonen "replace()" i Arduino](https://www.arduino.cc/reference/en/language/functions/string/replacement/)
- [En enkel guide til regulære uttrykk](https://www.regular-expressions.info/)
- [En mer dyptgående forklaring på søk og sletting av mønstre i tekststrenger](https://www.geeksforgeeks.org/remove-multiple-characters-from-string-in-arduino/)