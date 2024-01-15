---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Arduino: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter i Arduino programmering kan være nyttig for å gjøre ditt program mer fleksibelt og tilpasningsdyktig. Ved å ta inn og bruke kommandolinjeargumenter kan du enkelt endre inngangsparametere og variabler uten å måtte endre koden din.

## Hvordan

For å lese kommandolinjeargumenter i Arduino, kan du bruke funksjonen ```Arduino.parseInt()```. Denne funksjonen leser inn en numerisk verdi fra en streng som er gitt som et argument. Her er et enkelt eksempel på hvordan du kan bruke denne funksjonen:

```
int var = Arduino.parseInt(); // Leser inn en numerisk verdi og lagrer den i variabelen "var"
Serial.println(var); // Skriver ut den leste verdien til Serial monitor
```

Dette eksempelet vil lese inn et tall som blir gitt som et kommandolinjeargument når koden kjøres. For eksempel, hvis du bruker ```-123``` som et argument, vil verdien 123 bli skrevet ut til Serial monitor.

## Dypdykk

For mer avansert bruk av kommandolinjeargumenter i Arduino, kan du også lese inn strenger og flere parametere. Dette kan gjøres ved hjelp av funksjonen ```Arduino.readStringUntil()```, som leser inn en streng til et gitt tegn.

```
String inputString = ""; // Opprett en tom string for å lagre den leste verdien
char endChar = '\n'; // Sett et tegn som skal være sluttpunktet for den leste strengen
inputString = Arduino.readStringUntil(endChar); // Leser inn en streng frem til sluttpunktet er nådd
Serial.println(inputString); // Skriver ut den leste strengen til Serial monitor
```

Dette eksempelet vil lese inn en streng som blir gitt som et kommandolinjeargument, frem til sluttpunktet som er satt. Ved å sette ```-``` som begynnelsen på strengen, kan du for eksempel sende inn argumenter som ```-hello```, som vil bli lagret og skrevet ut som "hello" på Serial monitor.

## Se Også

For mer informasjon om kommandolinjeargumenter og hvordan du kan bruke dem i Arduino, kan du se disse ressursene:

- [Arduino.parseInt() dokumentasjon](https://www.arduino.cc/reference/no/language/functions/conversion/parseint/)
- [Arduino.readStringUntil() dokumentasjon](https://www.arduino.cc/reference/no/language/functions/communication/serial/readstringuntil/)
- [Video tutorial på bruk av kommandolinjeargumenter i Arduino](https://www.youtube.com/watch?v=L62KIb2ruYY)