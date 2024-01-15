---
title:                "Konvertere en dato til en streng."
html_title:           "Arduino: Konvertere en dato til en streng."
simple_title:         "Konvertere en dato til en streng."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Det kan være nyttig å konvertere en dato til en streng når du ønsker å vise datoen på en LCD-skjerm eller sende den på en seriell kommunikasjonslinje. Å konvertere datoen til en streng gjør det enklere å håndtere og formatere datoer i et program.

# Slik gjør du det
```Arduino
#include <TimeLib.h> // importer biblioteket for tidsfunksjoner

void setup() {
  Serial.begin(9600); // start seriell kommunikasjon med en baudrate på 9600
}

void loop() {
  // lag en stringvariabel for datoen og bruk funksjonen `dateTimeToString` for å konvertere den nåværende datoen til en streng
  String dato = dateTimeToString(now()); 
  Serial.print("Dagens dato er: ");
  Serial.println(dato); // skriv ut datoen på seriell monitor
  delay(1000); // vent i 1 sekund
}
```

Output:
```
Dagens dato er: 25/02/2020
```

# Dykk dypere
Når du konverterer en dato til en streng, bruker Arduino-biblioteket TimeLib en forhåndsdefinert formatstruktur som følger: "DD/MM/ÅÅÅÅ HH:MM:SS". Dette betyr at datoen vil bli vist på formatet dag/måned/år time:minutt:sekund. Du kan endre dette formatet ved å bruke funksjonen `setTimeFormat()`, og deretter inkludere et annet tegn eller symbol for å separere dato og tid.

# Se også
- TimeLib biblioteket: https://github.com/PaulStoffregen/Time
- Dato- og tidslære for Arduino: https://www.arduino.cc/reference/en/libraries/time/