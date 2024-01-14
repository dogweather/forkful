---
title:                "Arduino: Skriver til standardfeil"
simple_title:         "Skriver til standardfeil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error i Arduino-programmering er en viktig måte å feilsøke og rette opp i potensielle problemer i koden din. Ved å sende feilmeldinger til standard error, kan du få en bedre forståelse av hva som skjer i koden din og dermed forbedre ytelsen til prosjektet ditt.

## Hvordan gjøre det

Det er ganske enkelt å skrive til standard error i ditt Arduino-program. Du kan bruke funksjonen `Serial.print()` eller `Serial.println()` og sende meldingen du vil skrive som et argument i parentesene. For eksempel:

```Arduino
Serial.println("Dette er en feilmelding!");
```

Dette vil skrive meldingen "Dette er en feilmelding!" til standard error når koden kjøres. Du kan også sende variabler eller verdier, for å få mer spesifikk informasjon om problemene som oppstår. For eksempel:

```Arduino
int x = 10;
Serial.println("Verdien av x er: " + x);
```

Dette vil skrive ut verdien av variabelen "x" til standard error. Det er også viktig å huske på å aktivere seriell kommunikasjon i koden din ved å bruke `Serial.begin()`-funksjonen.

## Dypdykk

Å skrive til standard error kan være spesielt nyttig når du utvikler større og mer komplekse Arduino-prosjekter. Ved å bruke denne teknikken kan du enkelt finne og rette feil, i stedet for å bruke mye tid på å prøve å finne dem manuelt. Det er også et nyttig verktøy når du vil overvåke variabler og verdier mens koden din kjører. Så hvis du finner deg selv å støte på problemer i koden din, ikke glem å skrive til standard error for å få en bedre forståelse av hva som skjer.

## Se også

- [Arduino dokumentasjon om Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Bruk av Serial Monitor i Arduino IDE](https://www.arduino.cc/en/Guide/ArduinoSerialMonitor)
- [Feilsøking i Arduino-prosjekter](https://create.arduino.cc/projecthub/Arduino_Genuino/debugging-in-arduino-ide-805c11)