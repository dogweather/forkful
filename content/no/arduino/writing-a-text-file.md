---
title:    "Arduino: Å skrive en tekstfil"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil er en vanlig programvarefunksjon som kan være nyttig i mange ulike situasjoner. Det kan være nyttig for å lagre data, lage logger eller til og med lese og analysere større mengder med informasjon.

## Hvordan

For å skrive en tekstfil i Arduino, kan man bruke funksjonen "write()", som lar deg skrive en karakter eller en streng til filen. La oss se på et eksempel der vi skriver "Hei, verden!" til en tekstfil kalt "tekstfil.txt".

```Arduino

File myFile = SD.open("tekstfil.txt", FILE_WRITE); // Åpner eller oppretter filen

myFile.write("Hei, verden!"); // Skriver teksten til filen

myFile.close(); // Lukker filen

```

Når programmet kjøres, vil en tekstfil med navnet "tekstfil.txt" bli opprettet på SD-kortet og teksten "Hei, verden!" vil bli skrevet til filen. 

## Dypdykk

For å kunne skrive en tekstfil, må man først ha et SD-kort tilkoblet Arduino. Deretter må man åpne en fil ved å bruke funksjonen "open()" og angi at man ønsker å skrive til filen ved å inkludere "FILE_WRITE" som et argument. Det er også viktig å huske på å lukke filen ved å bruke "close()" når man er ferdig med å skrive til den.

Man kan også bruke en variabel i stedet for en fast tekst når man skriver til filen. For eksempel:

```Arduino

String navn = "Maria";

myFile.write(navn); // Skriver variabelen "navn" til filen

```

Dette gjør det mulig å legge inn variabler eller data som endres over tid i teksten som skrives til filen.

## Se også

- [Arduino-dokumentasjon om write() funksjonen](https://www.arduino.cc/reference/en/libraries/sd/write/)
- [Eksempelkode for å skrive en tekstfil på SD-kortet](https://www.arduino.cc/en/Tutorial/LibraryExamples/Write)