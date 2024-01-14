---
title:    "Arduino: Lesing av en tekstfil."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese tekstfiler kan være nyttig når man ønsker å hente informasjon fra en ekstern kilde, som for eksempel en sensor eller en database. Dette gjør det mulig å oppdatere variabler og data i et Arduino-program uten å måtte endre selve koden.

## Slik gjør du det

Det første trinnet for å lese en tekstfil i Arduino er å opprette en File-variabel ved hjelp av `File` objektet. Dette kan gjøres ved å bruke `open()` metoden og angi filnavnet. Etter dette kan man bruke `read()` metoden til å lese data fra filen og lagre den i en variabel. Her er et eksempel på hvordan man kan lese data fra en tekstfil og skrive den ut på serielinjen:

```Arduino
#include <SD.h>

File myFile;
char data;

void setup() {
  Serial.begin(9600);
  SD.begin(4);
  myFile = SD.open("data.txt");
  while (myFile.available()) {
    data = myFile.read();
    Serial.print(data);
  }
  myFile.close();
}

void loop() {

}
```

Dette eksempelet viser hvordan man kan lese innholdet fra filen "data.txt" som ligger på SD-kortet. Hver gang en ny karakter er lest, blir den skrevet ut på serielinjen. Husk å lukke filen når du er ferdig med å lese for å forhindre eventuelle problemer med SD-kortet.

## Dypdykk

Når man leser en tekstfil i Arduino, blir filen lest en byte om gangen. Dette betyr at man må vite hvilken type data man leser fra filen, for eksempel om det er tall eller bokstaver. Det er også viktig å sørge for at fila er formatert riktig, slik at dataene kan leses riktig.

En annen ting å huske på er at Arduino har begrenset RAM, så det kan være lurt å ikke lese for mye data på en gang for å unngå å fylle opp minnet. Man kan også bruke `available()` metoden for å sjekke om det er mer data å lese fra filen før man fortsetter å lese.

## Se også

- [Arduino SD Library](https://www.arduino.cc/en/Reference/SD)
- [SD.h Library Documentation](https://www.arduino.cc/en/Reference/SD)