---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å lese en tekstfil er prosessen å innhente og tolke informasjon lagret i en tekstfil fra programmeringsmiljøet. Det er viktig som det tillater programmerere å lagre og gjenbruke data på tvers av forskjellige prosjekter eller økter. 

## Hvordan:

Her er en enkel kode for å lese en tekstfil i Arduino:

```Arduino
#include <SD.h>

void setup()
{
  Serial.begin(9600);
  if (!SD.begin(10)) {
    Serial.println("initialization failed!");
    return;
  }
  File dataFile = SD.open("datalog.txt");
  if (dataFile) {
    while (dataFile.available()) {
      Serial.write(dataFile.read());
    }
    dataFile.close();
  }
}

void loop()
{
  // put your main code here, to run repeatedly:

}
```

Når du kjører denne koden, vil du se dataene fra "datalog.txt" i Serial Monitor.

## Dyp Dykk 

Historisk sett har lesing av tekstfiler vært en integrert del av programmering. Det startet med de tidligste lagringsenheter, som magnetbånd, og utviklet seg til å inkludere moderne hjelpemidler som SD-kort.

Det finnes også alternative måter å lese tekstfiler på. For eksempel kan du bruke EEPROM (Electrically Erasable Programmable Read-Only Memory) hvis du har mindre data å lagre og overføre mellom økter. Arduino har innebygget støtte for både SD-kort og EEPROM.

Når det gjelder å lese en tekstfil med Arduino, åpner SD.open() en fil på SD-kortet. Hvis filen er åpnet riktig, brukes Serial.write() for å skrive dataene i filen til Serieporten.

## Se Også:

For mer informasjon om dette emnet, sjekk ut disse kildene:

1. Arduino sin offisielle dokumentasjon på SD Library: https://www.arduino.cc/en/Reference/SD
2. EEPROM Library for Arduino: https://www.arduino.cc/en/Reference/EEPROM
3. Noen prinsipper om å lese og skrive til SD-kort med Arduino: https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial