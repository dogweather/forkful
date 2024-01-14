---
title:                "Arduino: Lesing av en tekstfil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å kunne lese en tekstfil er en viktig del av programmering, spesielt for Arduino. Det lar deg lagre og hente data fra en ekstern fil, som kan være nyttig for å lagre konfigurasjonsinnstillinger eller annen statisk informasjon.

## Hvordan
For å kunne lese en tekstfil på en Arduino må du først koble til en SD-kortleser. Deretter kan du bruke et bibliotek som f.eks. "SD.h" for å åpne og lese filen. Her er et eksempel på hvordan du kan lese en tekstfil og skrive ut innholdet til serielleskjermen:

```Arduino
#include <SPI.h>
#include <SD.h>

// Initialiserer SD-kortleseren
#define SD_CS 10
File myFile;

void setup() {
  // Åpner en serial tilkobling for å kunne skrive ut til serielleskjermen
  Serial.begin(9600);
  
  // Sjekker om SD-kortleseren er tilkoblet
  if (!SD.begin(SD_CS)) {
    Serial.println("SD-kortet kunne ikke leses");
    while (1);
  }
  
  // Åpner tekstfilen
  myFile = SD.open("tekstfil.txt");
  
  // Leser og skriver ut innholdet til serielleskjermen linje for linje
  while (myFile.available()) {
    Serial.println(myFile.readStringUntil('\n'));
  }
  
  // Lukker filen
  myFile.close();
}

void loop() {
  // Tomt, trenger ikke å gjenta lesingen av filen
}
```

Dette eksempelet viser hvordan du kan åpne og lese en tekstfil på en Arduino ved hjelp av et SD-kort.

## Dypdykk
Når du leser en tekstfil på en Arduino, er det viktig å ha i mente at mikrokontrolleren har begrenset med minne og prosesseringskraft. Derfor er det viktig å optimalisere koden for best mulig ytelse. Her er noen tips som kan hjelpe deg med å forbedre leseprosessen:

- Unngå å bruke for mange variabler. Hver variabel tar opp plass i minnet, derfor bør du begrense antallet variabler og sørge for å frigjøre minnet når du er ferdig med å bruke dem.
- Bruk "readStringUntil()" i stedet for "readString()". "readStringUntil()" lar deg lese en tekst til et spesifikt tegn, som kan hjelpe deg å unngå å lese hele filen og spare prosesseringskraft.
- Sørg for å lukke filen når du er ferdig med å lese den. Dette vil frigjøre ressurser og unngå potensielle problemer med å åpne den samme filen flere ganger.

Med disse tipsene kan du optimalisere koden din og forbedre ytelsen når du leser tekstfiler på din Arduino.

## Se også
- ["SD.h" biblioteket](https://www.arduino.cc/en/Reference/SD)
- [SD-kortleser tilkoblingsguide](https://www.arduino.cc/en/Guide/ArduinoSDCardShield)
- [Eksempel på å lese og skrive til et SD-kort](https://www.arduino.cc/en/Tutorial/Files)
- ["readStringUntil()" Arduino referanse](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)
- [Optimalisering av kode på en Arduino](https://www.arduino.cc/en/Reference/Optimize)