---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Substrenger er deler, eller segmenter, av en lengre tekststreng. Vi bruker substrings i programmering for å manipulere og analysere spesifikke deler av tekst, for eksempel å verifisere e-post format, finne og erstatte innhold, og mer.

## Slik Gjør Du:

Arduino tilbyr `substring()` funksjonen for å hente deler av en streng. Her er et eksempel:

```arduino
String melding = "Hallo, Verden!";
String del = melding.substring(7, 13);
Serial.println(del); // Skriver ut "Verden"
```

Her tar `substring()` funksjonen start og sluttposisjoner som parametre og returnerer teksten i den angitte rekkevidden.

## Dypdykk

`substring()` funksjonen i Arduino er blitt tilgjengelig siden Arduino 0019 versjonen. Det gir en enklere måte å manipulere strenger på, men det er også andre metoder som bruk av pekere, som kan være mer passende i mer komplekse scenarier.

Å jobbe med `substring()` funksjonen er ganske rett frem, men du må være oppmerksom på at indeksen begynner med 0, så det første tegnet i strengen er på posisjon 0.

## Se Også

For mer info og tips om strengbehandling i Arduino kan disse ressursene være til hjelp:

- Arduino sin offisielle dokumentasjon om `substring()`: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Guide til strengbehandling i Arduino: https://startingelectronics.org/software/arduino/learn-to-program-course/11-strings/
- Dybdykk i strenger og substrings: https://konekt.com/blog/2015/12/04/a-deep-dive-into-strings-in-arduino/