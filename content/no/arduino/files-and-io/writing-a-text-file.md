---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:05.765106-07:00
description: "\xC5 skrive en tekstfil i Arduino inneb\xE6rer \xE5 lagre data til en\
  \ fil p\xE5 et SD-kort eller lignende lagringsmodul, ofte for form\xE5l som datalogging.\u2026"
lastmod: '2024-03-11T00:14:14.668642-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive en tekstfil i Arduino inneb\xE6rer \xE5 lagre data til en fil\
  \ p\xE5 et SD-kort eller lignende lagringsmodul, ofte for form\xE5l som datalogging.\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil i Arduino innebærer å lagre data til en fil på et SD-kort eller lignende lagringsmodul, ofte for formål som datalogging. Programmerere gjør dette for å registrere sensoravlesninger, lagre konfigurasjoner, eller logge applikasjonshendelser over tid, noe som er avgjørende for prosjekter som krever dataanalyse eller sporing.

## Hvordan:
For å skrive til en tekstfil på et SD-kort ved hjelp av Arduino, trenger du først å inkludere `SD.h` biblioteket, som gir de nødvendige funksjonene for å samhandle med SD-kort. Sørg for at Arduino-kortet ditt er koblet til en SD-kortmodul.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Initialiser seriell kommunikasjon ved 9600 bits per sekund:
  Serial.begin(9600);
  
  // Sjekk for SD-kort initialisering
  if (!SD.begin(4)) {
    Serial.println("Initialisering feilet!");
    return;
  }
  Serial.println("Initialisering utført.");
  
  // Åpne filen. Merk at kun én fil kan være åpen om gangen,
  // så du må lukke denne før du åpner en annen.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Hvis filen ble åpnet ok, skriv til den:
  if (myFile) {
    Serial.print("Skriver til test.txt...");
    myFile.println("Tester tekstfilskriving.");
    // Lukk filen:
    myFile.close();
    Serial.println("ferdig.");
  } else {
    // Hvis filen ikke åpnet, skriv ut en feilmelding:
    Serial.println("Feil ved åpning av test.txt");
  }
}

void loop() {
  // Ingenting skjer etter oppsett
}
```

### Eksempel på utdata:
Når du kjører denne koden, vil Arduino IDE Serial Monitor vise:
```
Initialisering utført.
Skriver til test.txt...ferdig.
```
For å sjekke om dataen ble skrevet korrekt, kan du fjerne SD-kortet fra Arduinoen, sette det inn i en datamaskin, og åpne `test.txt`-filen for å se meldingen "Tester tekstfilskriving."

For prosjekter som krever mer avanserte filoperasjoner eller behandling, vurder å utforske tilleggsbiblioteker eller skrive egendefinerte funksjoner tilpasset dine spesifikke behov.
