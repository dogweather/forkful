---
title:                "Arduino: Konvertering av en streng til små bokstaver"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver kan være nyttig når man skal sammenligne og manipulere tekststrenger i et Arduino-program. Ved å gjøre alle bokstavene til små bokstaver, kan man lage en mer nøyaktig sammenligning og unngå feil.

## Hvordan

For å konvertere en streng til små bokstaver i Arduino, kan man bruke funksjonen `toLowerCase()` som er tilgjengelig i String-objektet. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```Arduino
String streng = "Hei, Dette ER en TEKSTSTRENG!";
streng.toLowerCase();
```

Dette vil endre strengen "streng" til å være "hei, dette er en tekststreng!". Merk at `toLowerCase()` funksjonen endrer den opprinnelige strengen og returnerer ikke en ny streng.

## Dypdykk

Å konvertere en streng til små bokstaver handler egentlig om å endre de store bokstavene til tilsvarende små bokstaver i ASCII-tabellen. Dette gjøres ved å legge til en bestemt verdi til hver bokstav. For eksempel, ved å legge til 32 til den desimale verdien til en stor bokstav, vil man få den tilsvarende lille bokstaven. Dette er også grunnen til at `toLowerCase()` funksjonen endrer bokstavene i den opprinnelige strengen.

Det er også viktig å merke seg at `toLowerCase()` funksjonen bare fungerer for engelske bokstaver, ikke for andre språk som bruker spesielle tegn eller diakritiske tegn.

## Se også

- [ASCII-tabellen](https://en.wikipedia.org/wiki/ASCII)
- [Offisiell dokumentasjon for String-objektet i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/)