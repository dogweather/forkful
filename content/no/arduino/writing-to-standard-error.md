---
title:                "Skriving til standardfeil"
html_title:           "Arduino: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å skrive til standard error i Arduino-programmering. Det kan være nyttig for å feilsøke og identifisere problemer, samt å gi en mer detaljert og strukturert utskrift av resultatene fra programmet.

## Hvordan

For å skrive til standard error i Arduino, bruker vi funksjonen `Serial.print()` og spesifiserer standard error som parameter. Her er et eksempel på hvordan dette kan gjøres:

```Arduino
Serial.print("Feil oppstod på rad 37", stderr);
```

Dette vil skrive ut teksten "Feil oppstod på rad 37" til standard error. Vi kan også kombinere dette med andre data, for eksempel:

```Arduino
int sensorValue = analogRead(A0);
Serial.print("Verdien fra analog sensor: ", stderr);
Serial.print(sensorValue, stderr);
```

Dette vil skrive ut teksten "Verdien fra analog sensor: " sammen med verdien fra sensoren til standard error.

## Dypdykk

Det er viktig å merke seg at når vi skriver til standard error, vil det ikke vises på den vanlige serielle monitoren i Arduino IDE. For å se resultatene må vi åpne "Seriel monitor" i menyen. Vi kan også endre størrelsen på Serial Buffer i Arduino-oppsettet for å øke eller redusere mengden data som kan skrives til standard error.

En annen ting å huske på er at når vi skriver til standard error, blir det ikke sendt tilbake til datamaskinen. Derfor kan det være lurt å kombinere bruk av `Serial.print()` med `Serial.read()` for å få resultatet tilbake og vise det på datamaskinen.

## Se også

- [Arduino Serial Monitor](https://www.arduino.cc/reference/en/language/functions/communication/serial/serialmonitor/)
- [Serial.print() dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)