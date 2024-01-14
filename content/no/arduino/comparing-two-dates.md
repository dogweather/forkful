---
title:                "Arduino: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en vanlig oppgave innenfor programmering, spesielt når det kommer til å håndtere tidspunkt og tidsbaserte funksjoner. Dette kan være nyttig for å undersøke forskjeller mellom datoer, beregne tidsintervaller, eller kontrollere om en dato er forbi eller fremdeles kommer. Ved hjelp av Arduino kan vi enkelt implementere en funksjon for å sammenligne to datoer.

## Slik gjør du det

For å sammenligne to datoer i Arduino, trenger vi å jobbe med variabler for år, måned og dag. Ved hjelp av disse variablene kan vi definere hver dato og deretter sammenligne dem ved å bruke de riktige betingelsene. La oss se på et eksempel hvor vi sammenligner to datoer og sjekker om den første datoen er tidligere enn den andre:

```
Arduino

int førsteDato = 2020/01/01; // Definerer første dato
int andreDato = 2020/01/15; // Definerer andre dato

if (førsteDato < andreDato) { // Sjekker om første dato er tidligere enn andre dato
  Serial.println("Første dato er tidligere enn andre dato!"); // Skriver ut en melding
}
```

I dette eksemplet bruker vi int (integer) for å definere datovariabler, men det kan også være nyttig å bruke andre datatyper som String, som kan være lettere å lese for en menneskelig bruker.

## Dypdykk

Det finnes ulike måter å sammenligne datoer på, avhengig av hva som er målet ditt. Noen ganger er det nok å bare sjekke om en dato er tidligere, senere eller lik en annen dato. Men i andre tilfeller må vi også ta hensyn til faktorer som tidsintervaller eller om det er et skuddår. Det er viktig å ha en grundig forståelse av datoformatet og ulike funksjoner for å kunne sammenligne datoer på en nøyaktig måte.

## Se også

- [Arduino Reference - Int](https://www.arduino.cc/reference/en/language/variables/data-types/sint/)
- [W3Schools - Arduino If...Else](https://www.w3schools.com/arduino/arduino_conditions.asp)
- [Norsk Arduino-forum](https://no.arduino.cc/forum/)