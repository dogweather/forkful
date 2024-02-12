---
title:                "Konvertere en streng til små bokstaver"
aliases:
- /no/arduino/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:45.131988-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver betyr å endre alle store bokstaver i teksten til små bokstaver. Programmerere gjør dette for enklere sammenligning av tekst, eller for å forenkle brukerinput.

## Slik gjør du:
Arduino-koden nedenfor viser hvordan du konverterer en streng til små bokstaver:

```arduino
void setup() {
  Serial.begin(9600); // Starter seriell kommunikasjon
  String tekst = "Hei Verden!";
  tekst.toLowerCase();
  Serial.println(tekst); // Skriver ut "hei verden!"
}

void loop() {
  // Her ville annen kode komme. Denne loop'en er tom.
}
```
Etter kjøring, ser du "hei verden!" i seriell monitor.

## Dypdykk:
Historisk sett har fullstendig tekstmanipulasjon vært nyttig for databehandling og kommunikasjon. Å konvertere en streng til små bokstaver er nyttig når man ignorerer casing i søk eller samler inn data der casing ikke skal telle. C++ `std::transform` er et alternativ du kan bruke på Arduino for lignende funksjonalitet. Implementeringsmessig bruker `toLowerCase()` metoden en løkke for å iterere gjennom hver karakter i strengen og konverterer dem ved hjelp av ASCII-verdier.

## Se Også:
- Arduino-referanse for `String`: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ `std::transform`: http://www.cplusplus.com/reference/algorithm/transform/
