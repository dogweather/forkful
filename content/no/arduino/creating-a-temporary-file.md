---
title:                "Arduino: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er nyttig når vi ønsker å lagre data midlertidig i vår Arduino-prosjekt. For eksempel kan vi bruke midlertidige filer for å lagre sensor data eller som et buffer for å lagre data før vi sender det til en annen enhet.

## Slik gjør du det

Opprettelse av midlertidige filer i Arduino er enkelt. Følg disse trinnene:

1. Inkluder SPIFFS biblioteket ved å skrive ```#include <SPIFFS.h>``` øverst i koden din.

2. Initialisere SPIFFS ved å skrive ```SPIFFS.begin()``` i ```setup``` delen av koden din.

3. Opprett en midlertidig fil ved hjelp av ```File tmpFile = SPIFFS.open("/midlertidig.txt", "w")```. Her legger vi til filnavnet og angir at filen skal skrives til. Merk at hvis filen allerede eksisterer, vil den gamle filen bli overskrevet med den nye.

4. Skriv data til filen ved hjelp av ```tmpFile.print()``` eller ```tmpFile.println()```.

5. Lukk filen ved å skrive ```tmpFile.close()```.

6. For å lese data fra filen, åpne den på nytt med ```SPIFFS.open("/midlertidig.txt", "r")``` og les data med ```tmpFile.read()``` eller ```tmpFile.readString()```.

## Dypdykk

Mens oppretting av midlertidige filer er enkelt, er det noen ting å vurdere når du bruker dem i Arduino-prosjektet ditt. Siden SPIFFS har begrenset lagringsplass, må du være forsiktig med hvor mye data du skriver til filen din. Husk også å slette filen når du er ferdig med å bruke den for å frigjøre plass på enheten din.

## Se også

* [SPIFFS biblioteket dokumentasjon](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html)
* [Eksempel på SPIFFS programmering](http://www.iotsharing.com/2017/09/how-to-use-spiffs-for-esp8266-to-store-files.html)
* [Alternative måter å lagre data på med Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)