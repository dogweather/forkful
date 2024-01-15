---
title:                "Skrive en tekstfil"
html_title:           "Arduino: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skrive en tekstfil på en Arduino? Det kan være mange grunner til å gjøre det, men hovedsakelig kan det være nyttig for å lagre data, lese sensorverdier eller lagre konfigurasjonsinnstillinger.

# Hvordan

Å skrive en tekstfil på en Arduino er en enkel prosess som kan gjøres ved hjelp av noen få linjer med kode. Følg disse trinnene for å lage en tekstfil med navnet "minfil.txt".

Først må du åpne serielmonitoren på Arduino-programmet for å kunne se utdataene fra koden. Deretter må du inkludere SD-biblioteket i koden din ved å skrive følgende linje:

```Arduino
#include <SD.h>
```

Deretter må du initialisere SD-kortet ved å skrive:

```Arduino
SD.begin(10); //der 10 er chip select-pinnen til SD-kortet
```

Nå kan du åpne en fil ved å skrive følgende kode:

```Arduino
File fil = SD.open("minfil.txt", FILE_WRITE);
```

Her åpnes en fil med navnet "minfil.txt" for skriving. Hvis filen ikke eksisterer, vil den bli opprettet.

Nå kan du skrive teksten du ønsker å lagre, ved å skrive:

```Arduino
fil.println("Dette er teksten som vil bli lagret i minfil.txt");
```

Til slutt må du lukke filen ved å skrive:

```Arduino
fil.close();
```

Etter å ha kjørt koden, kan du sjekke SD-kortet og du vil se at en fil med navnet "minfil.txt" har blitt opprettet og at teksten du har skrevet er lagret inne i den.

# Deep Dive

Det finnes flere forskjellige funksjoner man kan bruke når man skal skrive en tekstfil på en Arduino. Her er noen av de viktigste:

- `SD.begin ()` - initialiserer SD-kortet og returnerer true hvis det lykkes og false hvis det ikke gjorde det.
- `SD.open (filnavn, modus)` - åpner en fil med gitt filnavn og modus for lesing eller skriving.
- `fil.read ()` - leser en tegn fra filen og returnerer det som en byte.
- `fil.write (tegn)` - skriver et tegn til filen.

Det er viktig å huske at SD-kortet har begrensninger når det gjelder filnavn, lengde på filnavn og antall filer som kan være lagret på kortet. Det kan derfor være lurt å lese dokumentasjonen for ditt spesifikke SD-kort for å være sikker på at du følger riktig formatering.

# Se også

- [SD.h bibliotekets dokumentasjon](https://www.arduino.cc/en/Reference/SD)
- [Offisiell Arduino SD-kort tutorial](https://www.arduino.cc/en/Tutorial/ReadWrite)

For mer informasjon og detaljer om å skrive en tekstfil på en Arduino, kan du sjekke ut disse ressursene. Lykke til med kodingen!