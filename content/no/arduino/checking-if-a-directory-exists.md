---
title:    "Arduino: Sjekking av om en mappe eksisterer"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering med Arduino. Det kan hjelpe deg med å organisere og sikre dataene dine på en mer effektiv måte.

## Slik gjør du det

Det første du trenger å gjøre er å importere SD-kortbiblioteket. Dette gjøres ved å legge til følgende linjer på toppen av koden din:

```Arduino
#include <SD.h>
```

Deretter kan du opprette et objekt for SD-kortet ditt ved å bruke følgende linjer:

```Arduino
File root = SD.open("/");
if (root) {
  // do something
}
```

Nå kan du bruke `root`-objektet til å se etter en spesifikk mappe i SD-kortet ditt. For å sjekke om en mappe eksisterer, kan du bruke `exists()`-funksjonen som følger:

```Arduino
if (root.exists("/minmappe")) {
    // do something
}
```
Hvis mappen eksisterer, vil `exists()` returnere `true`, ellers vil den returnere `false`.

## Dykk ned i det

Hvis du vil utdype dine kunnskaper om å sjekke om mapper eksisterer, kan du også se på andre funksjoner som er tilgjengelige i SD-kortbiblioteket. For eksempel kan du bruke `openNextFile()`-funksjonen til å åpne neste fil eller mappe i en gitt mappe. Du kan også bruke `rewindDirectory()`-funksjonen til å restarte lesingen av en mappe fra begynnelsen.

En annen viktig ting å huske på er at SD-kortbiblioteket støtter både korte og lange filnavn, så du trenger ikke å bekymre deg for filnavnlengden når du sjekker mapper eller filer.

## Se også

- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Interacting with SD cards using Arduino](https://www.arduino.cc/en/Tutorial/ReadWrite)
- [Arduino and SD cards](https://learn.sparkfun.com/tutorials/arduino-and-sd-cards)