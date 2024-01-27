---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skrive til standard error (stderr) er å sende feilmeldinger separat fra vanlig utskriftsdata (stdout). Programmerere gjør dette for å logge feil uten å blande det med hoveddataflyten.

## Hvordan:
I Arduino-miljøet kan man ikke direkte skrive til stderr som på vanlige datamaskiner. Men vi kan bruke `Serial.print` til feilsøking:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Skriv ut feilmelding hvis knappetrykk oppdages (antatt på pin 2)
  int buttonState = digitalRead(2);
  if (buttonState == HIGH) {
    Serial.print("Feil: Knappen skal ikke være trykket nå!");
  }
  
  // Vanlig kode her ...

  delay(1000);
}
```
Seriemotoren vil vise feilmeldingen hvis tilstanden oppdages. Husk å åpne Serial Monitor for å se utskriftene.

## Dypdykk:
I tradisjonell programmering kan stderr brukes for å skille ut feil fra normal utskrift. På Arduino har vi ikke separasjon på samme måte, men Serial-kommunikasjonen hjelper til med feilsøking. Historisk sett har forskjellen på stdout og stderr vært viktig i Unix-baserte systemer. Uten støtte for stderr på Arduino, blir Serial Monitor og Serial Plotter vårt beste verktøy for diagnostisering.

## Se Også:
- Arduino Serial Library: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Feilsøking med Arduino: https://www.arduino.cc/en/Guide/Troubleshooting
- Innsikt i stderr og stdout: https://en.wikipedia.org/wiki/Standard_streams
