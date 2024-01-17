---
title:                "Sjekker om en mappe eksisterer"
html_title:           "Arduino: Sjekker om en mappe eksisterer"
simple_title:         "Sjekker om en mappe eksisterer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Sjekking av om en mappe eksisterer er en viktig del av programmering, spesielt når man jobber med filbehandling. Dette gjøres for å sikre at koden vår kjører på en forventet måte og for å unngå potensielle feil.

# Hvordan:
```
Arduino void setup() {
  if (SD.exists("mappenavn")) {
    Serial.println("Mappen eksisterer!");
  }
  else {
    Serial.println("Mappen eksisterer ikke!");
  }
}

void loop() {
  //Kode som kjører kontinuerlig
}
```

Når koden over kjører, vil den sjekke om mappen "mappenavn" eksisterer på SD-kortet. Hvis mappen eksisterer, vil den skrive ut "Mappen eksisterer!" i Serial Monitor, hvis ikke vil den skrive ut "Mappen eksisterer ikke!".

# Dypdykk:
Historisk sett har sjekking av eksisterende mapper vært viktig for å håndtere filsystemer med begrenset plass og ressurser. I dag er det fortsatt viktig for å sikre at filbehandlingen fungerer som den skal, spesielt når man håndterer store mengder data.

Alternativet til å sjekke eksisterende mapper er å bruke en try-catch blokk i koden for å håndtere eventuelle feil som oppstår hvis mappen ikke eksisterer. Dette kan være mer tidkrevende og mindre effektivt.

Implementering av sjekking av mapper eksisterer i flere ulike programmeringsspråk og er viktig å huske på når man jobber med filbehandling, uavhengig av om man bruker Arduino eller ikke.

# Se også:
[IODevice.isDir() – Arduino API Reference](https://www.arduino.cc/en/Reference/IO/isDir)
[Makes the SD library use 25MHz SPI interface](https://www.arduino.cc/en/Reference/IO/isDir)