---
title:                "Arduino: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

En viktig del av å programmere er å ha kontroll over filer og mapper som programmet trenger for å fungere riktig. Derfor er det viktig å kunne sjekke om en mappe eksisterer eller ikke, før man tar skritt for å åpne eller endre denne mappen. Dette kan spare deg for mye tid og frustrasjon når du jobber med Arduino.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer på en enkel måte, kan vi bruke funksjonen `exists()` i Arduino File System Library. La oss se på et eksempel:

```arduino
#include <FS.h>

void setup() {
  Serial.begin(9600);

  // Åpner en mappe som ikke eksisterer
  Dir dir = SPIFFS.openDir("/min_mappe");
  
  // Sjekker om mappen eksisterer
  if (dir.exists()) {
    Serial.println("Mappe finnes!");
  }
  else {
    Serial.println("Mappe finnes ikke!");
  }
}

void loop() {
  // no-op
}
```

Når du kjører dette programmet, vil du se at `Mappe finnes ikke!` blir skrevet til serieporten. Dette betyr at programmet har sjekket om mappen `/min_mappe` eksisterer, og det ble funnet ut at den ikke gjør det.

## Dypdykk

Når man sjekker om en mappe eksisterer, kan det også være lurt å vite om det er en fil eller en annen mappe med samme navn. I slike situasjoner kan man bruke `isFile()` og `isDir()` funksjonene for å få mer spesifikk informasjon om den gitte mappen.

```arduino
#include <FS.h>

void setup() {
  Serial.begin(9600);

  // Åpner en mappe som ikke eksisterer
  Dir dir = SPIFFS.openDir("/min_mappe");
  
  // Sjekker om mappen eksisterer og er en fil
  if (dir.exists() && dir.isFile()) {
    Serial.println("Dette er en fil!");
  }
  // Sjekker om mappen eksisterer og er en mappe
  else if (dir.exists() && dir.isDir()) {
    Serial.println("Dette er en mappe!");
  }
  else {
    Serial.println("Mappe finnes ikke!");
  }
}

void loop() {
  // no-op
}
```

I dette eksempelet vil output avhenge av hva slags fil eller mappe som finnes med navnet `/min_mappe`. Hvis det er en fil, vil `Dette er en fil!` bli skrevet ut, hvis det er en mappe, vil `Dette er en mappe!` bli skrevet ut, og hvis ingenting finnes, vil `Mappe finnes ikke!` bli skrevet ut.

Det er også verdt å merke seg at `exists()`, `isFile()`, og `isDir()` funksjonene returnerer en boolean-verdi (true eller false), og kan derfor brukes i sammenheng med if-setninger for å bestemme neste steg i programmet.

## Se også

- [Arduino File System Library](https://www.arduino.cc/en/Reference/ArduinoFS)
- [SPIFFS.openDir() documentation](https://github.com/esp8266/Arduino/blob/master/libraries/FS/src/FS.h#L92)
- [Eksempelkode for å sjekke om en fil eksisterer](https://techtutorialsx.com/2016/07/15/esp8266-arduino-check-if-a-file-exists-on-the-file-system/)