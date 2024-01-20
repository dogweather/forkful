---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å søke og erstatte tekst er prosessen med å identifisere spesifikk tekst i koden din og erstatte den med noe annet. Programvareutviklere bruker denne metoden for å endre variabler, oppdatere kommentarer og korrigere feil raskt.

## Hvordan gjør man det:
I Arduino kan du søke og erstatte tekst ved hjelp av replace() funksjonen. Her er en enkel kode for å demonstrere dette:

```Arduino
String tekst = "Hallo, Verden!";
tekst.replace("Verden", "Arduino");
Serial.println(tekst); 
```

Når du laster opp og kjører denne koden, vil utdataene være:

```Arduino
Hallo, Arduino!
```

## Dyp Dykk
Søking og erstatting tekst har vært et sentralt verktøy for programmerere siden tidlig utvikling av tekstbehandlingsprogrammer. I Arduino-miljøet kan du også bruke indexOf() funksjonen for å finne indexen til en bestemt tekst, før du bruker replace().

På samme måte kan du bruke substring() funksjonen til å erstatte en del av en streng, men dette kan bli mer komplisert enn nødvendig. Derfor er replace() vanligvis den foretrukne metoden.

Det er viktig å huske at replace() funksjonen endrer den opprinnelige strengen. Hvis du vil beholde originalen, kan du lage en kopi før du utfører erstatningen.

## Se Også
Arduino sin offisielle dokumentasjon tilbyr detaljert innsikt i replace() - funksjonen og relaterede metoder. Besøk [Arduino String Replace](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/) for mer info.