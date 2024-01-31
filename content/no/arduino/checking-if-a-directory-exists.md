---
title:                "Sjekke om en mappe finnes"
date:                  2024-01-19
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekking om en mappe finnes er metoden for å bekrefte at et bestemt katalogsti er tilgjengelig i filsystemet. Programmerere gjør dette for å unngå feil og for å sikre at filoperasjoner som lesing og skriving skjer problemfritt.

## Hvordan gjøre det:
Med Arduino kan du bruke SD-biblioteket for å interagere med filsystemet på et SD-kort. Her er hvordan du sjekker om en mappe eksisterer:

```Arduino
#include <SD.h>

void setup() {
    Serial.begin(9600);
    while (!Serial) {
        ; // vent for serial port å koble til
    }

    if (!SD.begin()) {
        Serial.println("Initialisering mislyktes!");
        return;
    }

    if (SD.exists("/minMappe")) {
        Serial.println("Mappen eksisterer!");
    } else {
        Serial.println("Mappen finnes ikke.");
    }
}

void loop() {
    // Ikke noe loop-arbeid nødvendig for denne oppgaven.
}
```
Etter kjøring av koden, hvis mappen finnes, vil du se "Mappen eksisterer!" i seriell monitor, og hvis den ikke finnes, "Mappen finnes ikke."

## Dypdykk:
Historisk har sjekk av katalog eksistens vært essensielt for mange operativsystemer. For embedded systemer som Arduino, ble dette behovet hentet inn med fremveksten av SD-kort-moduler som tillater datalagring. Alternativer til SD-biblioteket inkluderer SdFat-biblioteket, som også gir funksjoner for å sjekke eksistens av kataloger og filer med muligens mer avansert filhåndtering. Når det gjelder implementasjon har Arduino-enheter vanligvis begrensede ressurser, så det er viktig at sjekken er effektiv og tar liten plass i kode.

## Se Også:
- [Arduino SD-biblioteket referanse](https://www.arduino.cc/en/Reference/SD)
- [SdFat-bibliotek på GitHub](https://github.com/greiman/SdFat)
- [Arduino - Filsystem](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)
  
Med disse ressursene, kan du utforske mer om filhåndtering og ytterligere funksjoner tilgjengelige i Arduino for å arbeide med SD-kort og filsystemer.
