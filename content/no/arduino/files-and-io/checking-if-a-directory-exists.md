---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:46.658973-07:00
description: "Hvordan gj\xF8re det: Arduino st\xF8tter ikke nativt komplekse filsystemoperasjoner\
  \ rett ut av boksen. Imidlertid, med bruk av SD-biblioteket, som er en del av\u2026"
lastmod: '2024-03-13T22:44:41.072481-06:00'
model: gpt-4-0125-preview
summary: "Arduino st\xF8tter ikke nativt komplekse filsystemoperasjoner rett ut av\
  \ boksen."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan gjøre det:
Arduino støtter ikke nativt komplekse filsystemoperasjoner rett ut av boksen. Imidlertid, med bruk av SD-biblioteket, som er en del av det standard Arduino IDE, kan du enkelt arbeide med filer og mapper. For å sjekke om en mappe eksisterer, må du først initialisere SD-kortet og deretter bruke `exists()`-metoden fra SD-biblioteket.

Først, inkluder SD-biblioteket og deklarer chip select-pinnen:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Chip select-pin for SD-kortmodulen
```

I din `setup()`-funksjon, initialiser SD-kortet og sjekk om mappen eksisterer:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Initialisering mislyktes!");
    return;
  }

  // Sjekk om mappen eksisterer
  if (SD.exists("/myDir")) {
    Serial.println("Mappen eksisterer.");
  } else {
    Serial.println("Mappen eksisterer ikke.");
  }
}
```
I `loop()`-funksjonen, kan du holde den tom eller legge til annen operasjonell kode som nødvendig:

```cpp
void loop() {
  // Operasjonell kode eller holdes tom
}
```

Eksempel på utskrift etter å ha kjørt koden vil være enten:

```
Mappen eksisterer.
```
eller

```
Mappen eksisterer ikke.
```

Det er viktig å sørge for at SD-kortet er formatert korrekt, og at `/myDir` mappens sti samsvarer med dine spesifikke behov. Denne grunnleggende sjekken er en hjørnestein for å utføre mer komplekse operasjoner med filer og mapper på SD-kort med Arduino.
