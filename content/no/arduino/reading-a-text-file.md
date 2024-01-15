---
title:                "Å lese en tekstfil"
html_title:           "Arduino: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du ønsker å lagre og behandle større mengder data på Arduino-en din, er det å lese en tekstfil en nyttig ferdighet å ha. Dette kan åpne for flere muligheter for prosjekter og utvide hva du kan gjøre med Arduino. 

## Hvordan

Det første trinnet for å lese en tekstfil på Arduino er å åpne en seriell tilkobling. Deretter må du åpne filen ved hjelp av `SD.begin()` kommandoen. Bruk deretter `SD.open()` for å åpne filen og lagre en referanse til den i en variabel. Nå kan du bruke `file.read()` for å lese data fra filen og lagre den i en variabel. Husk å lukke filen med `file.close()` når du er ferdig for å unngå at det oppstår problemer.

```
Arduino.setup() {
    Serial.begin(9600);
    SD.begin(10);

    File file = SD.open("tekstfil.txt", FILE_READ);
    String data = file.read();
    Serial.print(data);
    file.close();
}
```

Når koden kjøres, vil teksten fra filen bli skrevet ut i serieporten. Du kan deretter behandle disse dataene på ønsket måte i koden din.

## Dypdykk

Det finnes flere mulige måter å lese en tekstfil på Arduino på, avhengig av hvilken versjon av Arduino du bruker og hvilket lagringsmedium du ønsker å lese fra. Det er også mulig å lese en tekstfil fra en ekstern enhet, som for eksempel en datamaskin, ved å opprette en seriell kommunikasjon mellom enhetene.

## Se også

- [SD-biblioteket for Arduino](https://www.arduino.cc/en/Reference/SD) 
- [Eksempelkode for lesing av tekstfiler på Arduino](https://www.arduino.cc/en/Tutorial/FileRead)