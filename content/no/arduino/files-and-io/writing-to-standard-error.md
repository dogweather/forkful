---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:31.047837-07:00
description: "Hvordan: Arduino skiller ikke innebygd mellom standard utdata og standard\
  \ feil slik konvensjonelle databehandlingssystemer gj\xF8r. B\xE5de `Serial.print()`\
  \ og\u2026"
lastmod: '2024-03-13T22:44:41.074598-06:00'
model: gpt-4-0125-preview
summary: "Arduino skiller ikke innebygd mellom standard utdata og standard feil slik\
  \ konvensjonelle databehandlingssystemer gj\xF8r."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
Arduino skiller ikke innebygd mellom standard utdata og standard feil slik konvensjonelle databehandlingssystemer gjør. Både `Serial.print()` og `Serial.println()` metodene skriver til samme serielle utdata, som vanligvis ses i Arduino IDE Seriell Monitor. Men vi kan etterligne skriving til stderr ved spesifikt å formatere feilmeldinger eller dirigere dem til et alternativt utdata, som en fil på et SD-kort eller over en nettverksforbindelse.

For å etterligne stderr, kan du prefikse feilmeldinger med en tag som "ERROR:" for å skille dem ut i Seriell Monitor:

```cpp
void setup() {
  Serial.begin(9600); // Initialiser seriell kommunikasjon på 9600 baud rate
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Etterligner stderr ved å prefikse feilmeldingen
    Serial.println("ERROR: Funksjonen feilet å utføre.");
  } else {
    Serial.println("Funksjonen ble utført vellykket.");
  }
  delay(1000); // Vent i ett sekund før du starter løkken på nytt
}

int someFunction() {
  // En dummy funksjon som returnerer -1 ved feil
  return -1;
}
```

Eksempelutdata i Arduino IDE Seriell Monitor kunne se slik ut:

```
ERROR: Funksjonen feilet å utføre.
```

For prosjekter som krever en mer sofistikert tilnærming, inkludert skriving til forskjellige fysiske utdata, kan bruk av tredjepartsbiblioteker eller ekstra maskinvare være nødvendig. For eksempel krever logging av feilmeldinger til et SD-kort `SD`-biblioteket:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: Initialisering av SD-kortet feilet!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: Funksjonen feilet å utføre.");
    myFile.close(); // Sørg for å lukke filen for å lagre innholdet
  } else {
    Serial.println("ERROR: Åpning av error.log feilet!");
  }
}

void loop() {
  // Din hovedkode ville gå her
}
```

Med denne tilnærmingen fysisk separerer du normale programutdata og feilmeldinger ved å dirigere sistnevnte til en `error.log` fil på et SD-kort, noe som muliggjør post-mortem analyser uten å forstyrre den primære utdatakanalen.
