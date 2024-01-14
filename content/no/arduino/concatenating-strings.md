---
title:                "Arduino: Sammenslåing av tekststrenger"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å sette sammen tekststrenger er en nyttig ferdighet når du jobber med Arduino-programmering. Ved å kombinere flere strenger kan du lage mer dynamiske og tilpassede utskrifter og brukerinteraksjoner. 

## Hvordan gjøre det
For å sette sammen tekststrenger i Arduino, kan du bruke funksjonen `strcat()`. Denne funksjonen tar to eller flere strenger som argumenter og kombinerer dem til én streng. Her er et eksempel på hvordan du kan bruke `strcat()`:

```Arduino 
char navn[20] = "Hilde";
char alder[3] = "25";

strcat(navn, " er ");
strcat(navn, alder);
strcat(navn, " år gammel.");

Serial.println(navn);
```

Dette vil resultere i en utskrift av `Hilde er 25 år gammel.` på serieporten. Det er viktig å merke seg at `strcat()` funksjonen endrer den første strengen, så det er nødvendig å bruke en midlertidig streng når du kombinerer mer enn to strenger. 

## Dypdykk
I tillegg til `strcat()`, er det andre måter å sette sammen strenger på i Arduino. Du kan for eksempel bruke `sprintf()` funksjonen, som lar deg inkludere variabler og konstanter i strengen. Her er et eksempel på hvordan du kan bruke `sprintf()`:

```Arduino
int tall = 10;

char tekst[20];
sprintf(tekst, "Tallet er: %d", tall);

Serial.println(tekst);
```

Dette vil resultere i en utskrift av `Tallet er: 10` på serieporten. `sprintf()` funksjonen gir deg også muligheten til å kontrollere antall siffer som skal inkluderes, og hvor de skal plasseres i strengen. 

## Se også
- [Official Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String concatenation tutorial](https://www.arduino.cc/en/Tutorial/StringAppendOperator)
- [sprintf() function documentation](https://www.cplusplus.com/reference/cstdio/sprintf/)