---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV står for "Comma-Separated Values". Det brukes for enkel lagring og utveksling av data, vanligvis fordi det er lett å forstå både av mennesker og maskiner.

## How to:
Arbeid med CSV-filer på Arduino kan involvere å lese eller skrive data. Her er et eksempel for å skrive til CSV:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10); 
  myFile = SD.open("data.csv", FILE_WRITE);

  // Hvis filen er åpen, skriv til den.
  if (myFile) {
    myFile.println("Temperatur,Humidity");
    myFile.println("22.5,45");
    myFile.println("23.0,47");
    myFile.close(); // Lukker filen etter bruk
  } else {
    // hvis filen ikke åpner, vis en feilmelding:
    Serial.println("Feil ved åpning av data.csv");
  }
}

void loop() {
  // Ikke noe loop-innhold for filskrivingseksemplet
}
```

## Deep Dive
CSV-formatet ble poppulært på grunn av sin enkelhet og har røtter tilbake til tidlig dataark-programvare på 1970-tallet. Alternativene til CSV inkluderer JSON og XML, som bærer mer datastruktur, men de kan kreve mer kompleks håndtering. På Arduino, må du huske på lagringsgrenser og velge riktig bibliotek (som `SD.h` for arbeid med SD-kort) for å lese/skrive CSV-filer.

## See Also
- Arduino SD Bibliotek Dokumentasjon: https://www.arduino.cc/en/Reference/SD
- CSV-standarden: https://tools.ietf.org/html/rfc4180
- "Serial Input Basics" forumstråden på Arduino: https://forum.arduino.cc/index.php?topic=396450
