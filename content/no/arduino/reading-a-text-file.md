---
title:                "Arduino: Leser en tekstfil"
simple_title:         "Leser en tekstfil"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i våre Arduino-prosjekter trenger vi å lese data fra en ekstern kilde som en tekstfil. Dette kan være for å hente sensoravlesninger, konfigurasjonsinnstillinger eller annen relevant informasjon. Å kunne lese en tekstfil åpner opp for flere muligheter i våre prosjekter og gjør dem mer dynamiske og tilpasningsdyktige. Derfor er det nyttig å lære hvordan man kan lese tekstfiler i Arduino-programmering.

## Hvordan gjøre det

Lesing av en tekstfil i Arduino krever noen få trinn, men er relativt enkelt og greit. Først må vi åpne filen ved å etablere en forbindelse mellom Arduino og datamaskinen. Deretter kan vi lese innholdet i filen og lagre det i variabler som vi kan bruke i koden vår.

La oss se på et eksempel der vi leser innholdet i en tekstfil som inneholder temperaturdata fra en sensor. Vi starter med å åpne en forbindelse til datafilen ved å bruke `SD.begin(pin)` -funksjonen. Dette gjør at Arduino kan kommunisere med SD-kortet som inneholder filen. Deretter åpner vi filen ved hjelp av `SD.open(nomefil, modus)` -funksjonen, der navnet på filen og lesemodusen må spesifiseres.

```Arduino
if(SD.begin(SS)) {
  myFile = SD.open("temperaturdata.txt", FILE_READ);
}
  ```
Nå som filen er åpnet, kan vi lese innholdet ved å bruke `myFile.read()` -funksjonen inne i en løkke. Vi lagrer resultatet i en variabel og behandler deretter dataene etter behov. Når vi er ferdige med å lese filen, lukker vi den ved å bruke `myFile.close()` -funksjonen.

```Arduino
while (myFile.available()) {
  int temperatur = myFile.read();
  // bruk temperaturdataene her
}
myFile.close();
```

Etter å ha behandlet dataene, kan vi bruke dem i vår Arduino-kode som vi ønsker. Det kan være å styre en motor, justere lysstyrken eller trigge en annen hendelse. Med kun noen få linjer med kode, kan vi nå få tilgang til data fra en ekstern tekstfil og bruke den i våre prosjekter.

## Dypdykk

Når vi leser en tekstfil, returnerer `myFile.read()` -funksjonen en byte av data om gangen. Dette betyr at hvis vi ønsker å lese mer enn én byte, må vi bruke en løkke for å lese og lagre hvert byte separat. I tillegg bør vi være oppmerksomme på at tekstfilene bør være riktig formatert for å kunne leses riktig av Arduino. Dette kan innebære å bruke spesielle tegn og tegnsett, eller å sørge for at dataene er lagret i riktig rekkefølge.

## Se også

- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino SD Card Module Tutorial](https://create.arduino.cc/projecthub/abdularbi17/sd-card-module-with-arduino-how-to-read-write-data-d60d5f)
- [How to Read and Write Files on an SD Card with an Arduino](https://www.circuito.io/blog/arduino-reading-and-writing-to-microsd-card/)