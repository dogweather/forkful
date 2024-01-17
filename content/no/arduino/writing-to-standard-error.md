---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Skriving til standardfeil er en måte for en programmerer å sende feilmeldinger til konsollen i stedet for seriell kommunikasjon. Dette kan være nyttig når man har mer komplekse programmer hvor man ønsker å logge viktig informasjon eller feilmeldinger.

# Slik gjør du:
```
// Kodingseksempler 

// Skrive melding til standardfeil:
Serial.write("Dette er en feilmelding");

// Les av standardfeil:
char buffer[64];
Serial.println(Serial.readBytes(buffer, 64));
```

# Dykke dypere:
Det å skrive til standardfeil ble først introdusert i Unix operativsystemet på 1970-tallet, og har siden blitt en vanlig praksis for å håndtere feilmeldinger i programmer. Alternativene til å skrive til standardfeil inkluderer å bruke et eget feilloggingsbibliotek eller å sende feilmeldinger til en egen skjerm.

Det å implementere skriving til standardfeil i Arduino er en enkel prosess, da det er innebygd støtte for dette i språket. Ved å bruke `Serial.write()` og `Serial.readBytes()` funksjonene, kan du enkelt sende og lese av meldinger fra standardfeil.

# Se også:
* Mer informasjon om skriving til standardfeil i Arduino: https://www.arduino.cc/reference/en/language/functions/communication/serial/write/
* Eksempler på bruk av skriving til standardfeil i Arduino: https://www.arduino.cc/en/Tutorial/SerialCallResponseASCII