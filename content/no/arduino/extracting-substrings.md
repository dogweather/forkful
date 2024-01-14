---
title:    "Arduino: Uttrekking av delstrenger"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen ønske å ekstrahere substringer i Arduino programmering? Vel, substringer er viktige når vi jobber med tekst og ønsker å manipulere eller hente ut deler av en tekststreng. Dette kan være nyttig når vi for eksempel ønsker å filtrere eller behandle data, eller når vi vil presentere informasjonen på en mer lesbar måte.

## Slik gjør du det

For å ekstrahere substringer i Arduino bruker vi funksjonen `substring()`. Denne funksjonen tar imot to argumenter: startindeks og sluttindeks. Dette avgrenser hvilke deler av tekststrengen vi ønsker å få ut.

For å illustrere dette, la oss si at vi har en tekststreng "Hei verden!" og vi ønsker å få ut "ei ve". Vi kan gjøre det på følgende måte:

```Arduino
String tekst = "Hei verden!";
String uttekst = tekst.substring(2,8);

Serial.println(uttekst);
```

Her bruker vi `substring()` funksjonen til å ekstrahere tekst fra indeks 2 (som tilsvarer bokstaven "e") til indeks 8 (som tilsvarer bokstaven "e" i "verden"). Resultatet vil være "ei ve" som blir skrevet ut i serieporten.

Det er viktig å merke seg at indeksene starter på 0 og ikke 1, så det første tegnet i tekststrengen har indeks 0.

Vi kan også bruke negative indekser for å ekstrahere fra slutten av tekststrengen. For eksempel, bruker vi `substring()` funksjonen med argumentene (-6,-2) på samme tekststreng som ovenfor, vil resultatet bli "verd".

## Dypdykk

I tillegg til startindeksen og sluttindeksen, kan `substring()` funksjonen også ta imot et tredje argument som indikerer lengden på substringen vi ønsker å få ut. For eksempel, hvis vi bruker argumentet 4, vil den hente ut de neste 4 tegnene fra startindeksen.

Vi kan også bruke variabler i stedet for tall som argumenter til `substring()` funksjonen. Dette gjør det mer fleksibelt og dynamisk når vi jobber med tekststrenger.

## Se også

For mer informasjon om `substring()` funksjonen og andre nyttige funksjoner når man jobber med tekststrenger i Arduino, kan du sjekke ut følgende ressurser:

- [Arduino reference for Substring](https://www.arduino.cc/reference/en/language/functions/communication/substring/)
- [Tutorial om tekstbehandling i Arduino](https://www.arduino.cc/en/Tutorial/StringIndexOf)
- [En guide til Arduino String-funksjoner](https://create.arduino.cc/projecthub/Arduino_Genuino/arduino-string-functions-1f5196)