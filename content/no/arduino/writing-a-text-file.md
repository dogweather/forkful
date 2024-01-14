---
title:                "Arduino: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive en tekstfil med Arduino kan være nyttig i mange sammenhenger. Det kan for eksempel være en måte å lagre sensoravlesninger eller annen data for senere bruk, eller å kommunisere med et eksternt display eller lagringsenhet. Uansett hva grunnen måtte være, er det viktig å vite hvordan man gjør det riktig.

## Hvordan

Først må du åpne et nytt Arduino-prosjekt og importere biblioteket "SD" ved å skrive følgende kode i starten av skissen din:

```Arduino
#include <SD.h>
```

Deretter må du initialisere SD-kortet ved å skrive følgende kode:

```Arduino
if (!SD.begin(chipSelect)) {
  Serial.println("Feil ved initialisering av SD-kortet");
  return;
}
```

Her erstatter du "chipSelect" med den pinnen du har valgt å bruke for å kommunisere med SD-kortet ditt. Deretter kan du begynne å skrive til tekstfilen ved å åpne en filstrøm og bruke "println" -funksjonen til å skrive det du vil til tekstfilen:

```Arduino
File dataFile = SD.open("data.txt", FILE_WRITE);
if (dataFile) {
  dataFile.println("Dette er en test");
  dataFile.close();
}
```

Når du er ferdig, må du huske å lukke filstrømmen og SD-kortet ved å bruke følgende kode:

```Arduino
dataFile.close();
SD.end();
```

### Eksempel 1: Lagring av sensoravlesninger

Dette eksempelet viser hvordan du kan lagre sensoravlesninger til en tekstfil på SD-kortet. Først må du konfigurere sensoren og initialisere SD-kortet som vist ovenfor. Deretter kan du lese av sensoren og lagre verdiene i en variabel. Til slutt skriver du verdien til tekstfilen.

### Eksempel 2: Kommunikasjon med eksternt display

Hvis du ønsker å kommunisere med et eksternt display ved hjelp av en tekstfil, kan du følge trinnene ovenfor for å skrive til tekstfilen og deretter lese av filen og vise innholdet på displayet.

## Dypdykk

Når du skriver en tekstfil med Arduino, må du være oppmerksom på noen viktige ting. For det første må du sørge for at du har valgt riktig pinne for å kommunisere med SD-kortet ditt. Hvis du velger feil pinne, vil ikke koden fungere. For det andre må du passe på å lukke filstrømmen og SD-kortet når du er ferdig for å unngå eventuelle problemer med å lese og skrive til filen senere.

En annen ting å huske på er at SD-kortet må være formatert som FAT16 eller FAT32 for at Arduino skal kunne lese og skrive på det. Hvis du har problemer med å få koden til å fungere, kan det være lurt å kontrollere at SD-kortet ditt er formatert riktig.

## Se også

- [Arduino SD-bibliotek](https://www.arduino.cc/en/reference/SD)
- [Tutorial: SD-kort og Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)