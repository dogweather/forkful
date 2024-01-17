---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Arduino: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor
Lesing av kommandolinje-argumenter er en måte for programmerere å hente inn informasjon som er gitt ved kjøretid. Dette kan være nyttig for å tilpasse programmer til forskjellige bruksscenarier, som for eksempel ulike innstillinger eller filer som skal leses. 

## Hvordan gjør man det:
```Arduino
void setup() {
  Serial.begin(9600); // Setter baudrate for seriekommunikasjon
  while (!Serial) {} // Venter på at serieporten skal bli tilgjengelig
  if (Serial.available()) { // Sjekker om det finnes data i seriebufferen
    String input = Serial.readStringUntil('\n'); // Leser seriell data inntil ny linje
    Serial.print("Du skrev: ");
    Serial.println(input); // Skriver ut input på seriell monitor
  }
}

void loop() {
  // Tom loop-funksjon, fordi vi ikke gjør noe i kontinuerlig kjøremodus
}
```
**Eksempel på seriekommando:** ```Arduino
Du skrev: Hei verden!
```

## Dykk dypere
Lesing av kommandolinje-argumenter har vært en vanlig programmeringsteknikk siden starten av datamaskinens tid. I dag finnes det flere alternativer for å lese inn data, som for eksempel ved hjelp av grafiske brukergrensesnitt eller konfigurasjonsfiler. 

Implementasjonen av lesing av kommandolinje-argumenter varierer mellom ulike programmeringsspråk og plattformer. I Arduino, som er basert på C++, kan man bruke funksjonen ```Serial.readStringUntil()``` for å lese data fra serieporten. Det er også viktig å inkludere en sjekk for tilgjengelig data, for å unngå feil ved kjøring. 

## Se også
- [Offisiell Arduino dokumentasjon for ```Serial.readStringUntil()```](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)
- [En guide til å lese kommandolinje-argumenter i C++](https://www.cprogramming.com/tutorial/lesson14.html)