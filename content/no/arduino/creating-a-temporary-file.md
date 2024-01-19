---
title:                "Opprette en midlertidig fil"
html_title:           "Arduino: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil gir programmet muligheten til å lagre data midlertidig. Dette er nyttig når du behandler store mengder data som ikke passer inn i RAM, eller når du må dele data mellom forskjellige økter av en app.

## Hvordan:
La oss lage en temp fil i Arduino med SD-kortmodulen:

```Arduino
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Initialization Failed!");
    return;
  }
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  if (tempFile) {
    tempFile.println("This is a temp file!");
    tempFile.close();
    Serial.println("Temp file write done.");
  } else {
    Serial.println("Error opening temp.txt");
  }
}

void loop() {
  // put your main code here, to run repeatedly:
}
```

Output blir: `"Temp file write done."`

## Dypdykking
Historisk sett har programmerere alltid hatt behov for å lagre data midlertidig, og midlertidige filer har vært løsningen. Men, det er noen alternativer nå:

- Bruke databasesystemer som SQLite i stedet for midlertidige filer.
- Bruke innebygd EEPROM for små lagringsbehov.

Å lage en midlertidig fil i Arduino innebærer å bruke SD.h-biblioteket, som tillater lesing og skriving til SD-kortet. 

## Se Også
For mer informasjon om hvordan du jobber med filer og SD-kort i Arduino, besøk disse kildene:

- [Arduino SD-biblioteket](https://www.arduino.cc/en/reference/SD)
- [SD-kortmodulveiledning](https://lastminuteengineers.com/arduino-sd-card-module-tutorial/)
- [Lær mer om EEPROM](https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROM)