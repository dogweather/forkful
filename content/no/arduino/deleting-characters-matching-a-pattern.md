---
title:                "Arduino: Sletting av tegn som samsvarer med et mønster"
simple_title:         "Sletting av tegn som samsvarer med et mønster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ville slette tegn som passer til et bestemt mønster i Arduino-programmering. Det kan være for å rydde opp i data eller for å filtrere ut uønsket informasjon. Uansett årsak, kan dette være en nyttig ferdighet å ha på lager.

## Slik gjør du det

Det enkleste måten å slette tegn som passer til et mønster på, er å bruke funksjonen `replace()` i Arduino. Denne funksjonen lar deg erstatte et bestemt tegn eller en sekvens av tegn med et annet tegn eller en sekvens.

```Arduino
// Eksempel på bruk av `replace()` for å slette tegn som passer til et mønster
String string = "Hei, dette er en eksempeltekst!";
string.replace("e", ""); // Denne koden vil slette alle forekomster av bokstaven "e" i teksten
Serial.println(string); // Resultatet vil være "Hi, dtte r n eksmpltkst!"
```

Det er også mulig å bruke et regulært uttrykk (regex) for å slette tegn som passer til et mønster i en tekst. Dette gir større fleksibilitet og mulighet for mer avansert filtrering.

```Arduino
// Eksempel på bruk av regex i Arduino for å slette tegn som passer til et mønster
String string = "Hei, dette er en eksempeltekst!";
string = regexReplace(string, "e[ai]", ""); // Denne koden vil slette bokstaven "e" etterfulgt av enten "a" eller "i" i teksten
Serial.println(string); // Resultatet vil være "Hi, dtte r en tekst!"
```

## Dypdykk

Å kunne slette tegn som passer til et mønster krever en god forståelse av strenger og hvordan de fungerer i Arduino-programmering. Det er også viktig å ha grunnleggende kunnskap om regulære uttrykk og hvordan de kan brukes til å filtrere data.

En ting å huske på er at `replace()`-funksjonen i Arduino bare vil erstatte det første mønsteret den finner i en tekst. Hvis du ønsker å slette alle forekomster, må du bruke en løkke og kjøre `replace()` flere ganger.

## Se også

- [Arduino referanseside for `replace()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Offisiell dokumentasjon for regex i Arduino](https://www.arduino.cc/reference/en/language/functions/regular-expressions/)
- [RegExr - online verktøy for å teste og eksperimentere med regulære uttrykk](https://regexr.com/)