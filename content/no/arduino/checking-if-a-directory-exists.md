---
title:                "Arduino: Sjekke om en mappe eksisterer"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av Arduino-programmering, spesielt når man jobber med et større prosjekt. Dette er fordi det kan bidra til å organisere og strukturere koden bedre, og sikre at alt kjører som det skal.

## Slik gjør du det

For å sjekke om en mappe eksisterer i Arduino, kan du bruke funksjonen `exists()` fra File-biblioteket. Her er et eksempel på hvordan du kan bruke denne funksjonen for å sjekke om en mappe med navnet "Bilder" eksisterer:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  // Åpne Serial Monitor for å se utskrifter fra Arduino
  Serial.begin(9600);

  // Initialiser SD-kortet
  SD.begin(10);

  // Sjekk om mappen "Bilder" eksisterer
  if (SD.exists("/Bilder")) {
    Serial.println("Mappen Bilder eksisterer!");
  } else {
    Serial.println("Mappen Bilder eksisterer ikke...");
  }
}

void loop() {
  // Ingenting trenger å gjøres i hovedsløyfen
}
```

Når dette eksempelet kjøres, vil Serial Monitor vise enten "Mappen Bilder eksisterer!" eller "Mappen Bilder eksisterer ikke...", avhengig av om mappen eksisterer eller ikke.

## Dypdykk

Når du bruker funksjonen `exists()`, er det viktig å merke seg at den kan returnere både `true` og `false` selv om mappen tilsynelatende eksisterer. Dette kan skyldes filrettigheter eller at SD-kortet ikke er riktig formatert. Det kan også være lurt å inkludere betingelser som sjekker om SD-kortet er tilgjengelig og om du har skrivetilgang til mappen.

## Se også

- [File Library Reference](https://www.arduino.cc/en/Reference/File)
- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [How to Use Libraries in Arduino](https://www.arduino.cc/en/Guide/Libraries)