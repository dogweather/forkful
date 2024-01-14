---
title:    "Arduino: Konvertere en dato til en streng"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å konvertere et datofelt til en tekststreng i Arduino-programmering. Kanskje du trenger å logge datoer i en fil, eller kanskje du vil vise datoinformasjon på en LCD-skjerm. Uansett hva motivasjonen din er, er det viktig å kunne konvertere datoer til tekst for å kunne jobbe med dem i koden din.

## Hvordan

For å konvertere en dato til en tekststreng, trenger du å bruke et bibliotek som heter "Time". Dette biblioteket lar deg enkelt få tilgang til dato- og tidsinformasjon på Arduino-en din. La oss ta en titt på et eksempel på hvordan du kan konvertere en dato til en tekststreng:

 ```Arduino
#include <Time.h> // inkluderer Time biblioteket

void setup() {
  Serial.begin(9600); // starter seriell kommunikasjon for debugging
}

void loop() {
  // Henter nåværende dato og tid
  time_t datoOgTid = now(); 
  // Konverterer dato og tid til en tekststreng med formatet "dd/mm/yyyy hh:mm:ss"
  String datoTekst = String(day(datoOgTid)) + "/" + String(month(datoOgTid)) + "/" + String(year(datoOgTid)) 
  + " " + String(hour(datoOgTid)) + ":" + String(minute(datoOgTid)) + ":" + String(second(datoOgTid));
  
  Serial.println(datoTekst); // skriver ut den konverterte tekststrengen til seriell monitor
  delay(1000); // venter 1 sekund før loop restartes
}
```
Når du laster opp denne koden til Arduino-en din, vil du se at dagens dato og tid blir skrevet ut i seriell monitor. Du kan også endre på formateringen eller legge til annen informasjon som måned eller ukedag, ved å se på dokumentasjonen til Time biblioteket.

## Dypdykk

Nå som du har et grunnleggende eksempel på hvordan du kan konvertere en dato til en tekststreng, kan det være nyttig å forstå hvordan denne koden fungerer. Først inkluderer vi biblioteket "Time" for å få tilgang til funksjoner for dato og tid. Deretter starter vi seriell kommunikasjon som lar oss kommunisere med datamaskinen og skrive ut informasjonen vi ønsker.

I loop-funksjonen henter vi først nåværende dato og tid ved hjelp av funksjonen "now()". Dette returnerer et time_t-objekt med informasjon som vi kan jobbe med. Deretter bruker vi funksjoner som day, month, year, hour, minute og second til å hente ut de ulike delene av datoen og tidspunktet. Disse funksjonene returnerer tallverdier, så vi må bruke String-funksjonen for å konvertere dem til tekst.

Til slutt bruker vi konkatenering (+) til å kombinere disse tallverdiene og sette dem i riktig format for en tekststreng med datoinformasjon. Ved å bruke denne metoden, kan du enkelt tilpasse formatet etter dine behov.

## Se også

For mer informasjon om å jobbe med datoer og tid i Arduino-programmering, kan du sjekke ut disse ressursene: 

- [Offisiell dokumentasjon for Time biblioteket](https://www.arduino.cc/en/Reference/Time)
- [Arduino tutorial om tid og dato](https://www.arduino.cc/en/Tutorial/Time)
- [YouTube video om å konvertere datoer til tekststrenger på Arduino](https://www.youtube.com/watch?v=J1vaIIrJ3RI)