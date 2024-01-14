---
title:                "Arduino: Å skrive en tekstfil"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor: Skriving av tekstfiler med Arduino

Å skrive en tekstfil med Arduino kan være veldig nyttig for å lagre variabler, data eller sensoravlesninger. Tekstfiler kan også brukes til å kommunisere med andre enheter eller systemer, for eksempel en datamaskin. Ved å kunne skrive tekstfiler med Arduino kan du utvide funksjonaliteten og bruksområdene til prosjektet ditt.

## Slik gjør du det: Eksempelkoding og utdata

For å skrive en tekstfil med Arduino trenger du en SD-kortmodul og et SD-kort. Først må du initialisere SD-kortet og åpne en fil med ønsket navn ved hjelp av "SD.begin()" og "SD.open()" funksjonene. Deretter kan du skrive til filen ved hjelp av "SD.write()" eller "SD.println()" funksjonene. Her er en enkel kode som illustrerer dette:

```Arduino
#include <SPI.h>
#include <SD.h>

File file; // Definerer en fil

void setup() {
  // Sett opp seriell kommunikasjon og initialiser SD kortet
  Serial.begin(9600);
  while (!Serial) {} // Venter på tilkobling

  Serial.print("Initialisering av SD-kort...");
  if (!SD.begin(4)) { // Bruker pin 4 til å kommunisere med SD-kortmodulen
    Serial.println("Initiering mislyktes!");
    return;
  }
  Serial.println("SD-kortet er klart.");

  // Åpner en fil med navnet "data.txt"
  file = SD.open("data.txt", FILE_WRITE);
  if (file) {
    Serial.println("Fil åpnet.");
  }
}

void loop() {
  // Skriver til filen og skriver ut til seriell kommunikasjon
  file.println("Dette er en test.");
  Serial.println("Dette er en test.");

  delay(500);

  // Lukker filen
  file.close();

  // Starter på nytt og åpner filen igjen
  // Dette vil overskrive det som allerede finnes i filen
  file = SD.open("data.txt", FILE_WRITE);
}
```

Etter å ha lastet opp koden til Arduino, kan du åpne seriell monitor for å se utdataen. Du bør se at teksten blir skrevet både til filen og til seriell monitor.

## Dykk dypere: Mer informasjon om skriving av tekstfiler

Merk at kodeeksempelet ovenfor kun illustrerer det grunnleggende for å skrive en tekstfil med Arduino. Det finnes mange flere funksjoner og metoder som kan brukes til å håndtere tekstfiler mer avansert, for eksempel å lese eller slette innhold i filen. Det kan være lurt å lese dokumentasjonen til biblioteket du bruker for å skrive tekstfiler med Arduino for å få en dypere forståelse av mulighetene.

## Se også

- [SD-biblioteket for Arduino](https://www.arduino.cc/en/Reference/SD)
- [SD-kortmodulen](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Bibliotek for å håndtere tekstfiler](https://www.arduino.cc/en/Reference/FileIO)