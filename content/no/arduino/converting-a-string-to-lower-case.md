---
title:    "Arduino: Konvertere en streng til små bokstaver"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er nyttig når man ønsker å sammenligne eller prosessere tekst. Ved å ha alt i samme format, blir det enklere å finne og manipulere bestemte deler av teksten. Dette kan være nyttig i ulike applikasjoner, som for eksempel å filtrere data eller gjøre tekstbehandling.

## Hvordan

For å konvertere en streng til små bokstaver i Arduino, kan man bruke den innebygde funksjonen `toLowerCase()`. Denne funksjonen tar inn en streng som parameter og returnerer den samme strengen i små bokstaver. Her er et eksempel på hvordan man kan bruke denne funksjonen i en enkel Arduino-sketch:

```Arduino
String tekst = "Hei, dette er en Tekst";
Serial.println(tekst); // Skriver ut "Hei, dette er en Tekst"

tekst.toLowerCase(); // Konverterer teksten til små bokstaver
Serial.println(tekst); // Skriver ut "hei, dette er en tekst"
```

Som du kan se, er det enkelt å konvertere en streng til små bokstaver ved hjelp av denne funksjonen. Det er viktig å huske på at denne funksjonen kun konverterer bokstaver, og eventuelle spesialtegn eller tall vil fortsatt være i samme format som før.

## Dypdykk

Når man konverterer en streng til små bokstaver, er det viktig å være oppmerksom på språket som brukes. Dette er fordi noen språk har ulike måter å konvertere bokstaver på. For eksempel vil en streng på tysk ha en annen konvertering til små bokstaver enn en streng på engelsk.

I tillegg, hvis man ønsker å konvertere en streng til store bokstaver, kan man bruke funksjonen `toUpperCase()`. Denne funksjonen fungerer på samme måte som `toLowerCase()`, men konverterer teksten til store bokstaver i stedet. Det er også verdt å merke seg at disse funksjonene kun fungerer på ASCII-tegn, og eventuelle unicode-tegn vil ikke bli konvertert.

## Se også

For mer informasjon om Arduino-programmering og strengmanipulasjon, sjekk ut disse ressursene:

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [W3Schools String Functions](https://www.w3schools.com/cpp/cpp_strings_functions.asp)
- [Arduino String handling tutorial](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)

Jeg håper denne artikkelen har vært nyttig for å forstå hvordan man kan konvertere en streng til små bokstaver i Arduino. Lykke til med din neste prosjekt!