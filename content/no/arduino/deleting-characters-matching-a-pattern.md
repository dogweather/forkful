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

## Hvorfor

Det er flere grunner til at noen kan ønske å slette tegn som matcher et spesifikt mønster i en Arduino-programmering. Dette kan være for å effektivisere kode, spare plass i minnet, eller for å forenkle behandlingen av data.

## Hvordan

Å slette tegn som matcher et mønster kan gjøres ved hjelp av string-funksjonene `substring()` og `replace()`. Først må en string-variabel opprettes og fylles med tekst. Deretter brukes `substring()` til å hente ut deler av stringen basert på en start- og sluttposisjon. Det som da gjenstår av stringen er det man ønsker å beholde. Deretter brukes `replace()` til å erstatte den originale stringen med det som gjenstår. Her er et eksempel på bruk av disse funksjonene:

```Arduino
String originalTekst = "Hello World";
String nyTekst = originalTekst.substring(1, 4);
nyTekst.replace("l", "");

// nyTekst vil nå være "Heo Word"
```

## Dypdykk

En viktig ting å være klar over når man sletter tegn er at det kan påvirke lengden på stringen. Dette kan være nyttig å vite når man jobber med andre funksjoner som bruker lengden på en string som parameter. Det kan derfor være lurt å lagre lengden på stringen før man sletter tegn, slik at man kan bruke dette senere hvis det trengs.

Et annet viktig poeng er at `substring()` og `replace()` bare jobber med enkelttegn. Hvis man ønsker å slette flere tegn som matcher et mønster, må man bruke en løkke og sjekke hvert tegn individuelt.

## Se Også

- [Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arduino String Documentation](https://www.arduino.cc/en/Reference/StringObject)