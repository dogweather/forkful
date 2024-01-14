---
title:                "Arduino: Lesing av kommandolinjeargumenter"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinje-argumenter er en viktig ferdighet å ha som Arduino-programmerer. Det hjelper deg å lage mer interaktive og dynamiske programmer som kan tilpasses av brukeren. Hvis du ønsker å ta Arduino-programmeringen din til neste nivå, er det absolutt verdt å lære hvordan du kan lese kommandolinje-argumenter.

## Slik gjør du det

For å lese kommandolinje-argumenter i Arduino, må du bruke Serial Monitor-funksjonen. Først må du åpne Serial Monitor ved å klikke på "Tools" og deretter "Serial Monitor" i Arduino-IDE-en. Når vinduet åpner seg, skriv inn "Serial.printline()" i loop-funksjonen din, etterfulgt av variabelen eller dataen du ønsker å lese.

````Arduino
int verdi = Serial.parseInt();
Serial.println("Verdien du skrev inn er: " + verdi);
````

Når du laster opp koden til Arduino-en din og åpner Serial Monitor, vil du kunne skrive inn kommandolinje-argumenter og se resultatet i Serial Monitor-vinduet.

## Dykk dypere

Det er viktig å merke seg at kommandolinje-argumenter må være plassert etter kompilerte koden i Arduino-IDE-en. Dette påvirker også rekkefølgen på kommandolinje-argumenter hvis du skriver inn mer enn én separert med mellomrom.

En annen ting å huske på er at Arduino kun kan lese ASCII-tegn i kommandolinje-argumenter. Dette betyr at ikke-tekstlige verdier, som for eksempel desimaltall, må konverteres til tekst før de kan leses av Arduino.

## Se også

* [Serial.printline() Dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
* [Tutorial: How to Read Command Line Arguments in Arduino](https://lastminuteengineers.com/how-to-read-arduino-cli-arguments/)
* [Understanding Command Line Arguments in Arduino IDE](https://iotlaboratory.wordpress.com/2016/11/05/understanding-command-line-arguments-in-arduino-ide/)