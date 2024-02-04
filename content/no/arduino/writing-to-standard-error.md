---
title:                "Skriving til standardfeil"
date:                  2024-02-03T19:32:31.047837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å skrive til standard feil (stderr) i Arduino-programmering innebærer å dirigere feilmeldinger og diagnostikk til en separat kanal, slik at de ikke blandes med standard utdata (stdout). Programmerere gjør dette for å skille normale programutdata fra feilmeldinger, noe som gjør feilsøking og logganalyse mer rett frem.

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
