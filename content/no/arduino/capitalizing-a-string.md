---
title:                "Store bokstaver i en tekststreng"
html_title:           "Arduino: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer kanskje på hvorfor noen ville engasjere seg i å gjøre en streng stor eller liten bokstav? Vel, det kan være nyttig i mange situasjoner. Kanskje du vil gjøre brukerinput til en konstant for å sikre ensartethet, eller kanskje du trenger å matche en streng med en annen som er skrevet i en annen form. Uansett hva årsaken er, er det enkelt å gjøre med Arduino-programmering.

## Hvordan gjøre en streng stor eller liten bokstav

For å gjøre en streng stor bokstav, kan du bruke funksjonen .toUpperCase(). For å gjøre en streng liten bokstav, kan du bruke funksjonen .toLowerCase(). For å gjøre dette, må du først lagre strengen du vil endre i et variabelnavn. Deretter bruker du funksjonen og lagrer den nye verdien i et annet variabelnavn. Her er et eksempel på hvordan dette kan se ut i Arduino-kode:

```Arduino
String tekst = "Dette er en tekst";
String storBokstav = tekst.toUpperCase();
String litenBokstav = tekst.toLowerCase();
Serial.print(storBokstav); // Utskrift: "DETTE ER EN TEKST"
Serial.print(litenBokstav); // Utskrift: "dette er en tekst"
```

## Dypdykk

Når du bruker .toUpperCase() og .toLowerCase(), må du være oppmerksom på at dette bare endrer bokstavenes case, og ikke selve bokstavene. Dette betyr at en bokstav som "Ø" i norsk vil bli gjort om til "Ø" og ikke "ø". Dette er fordi ASCII-koden for "Ø" og "ø" er forskjellig. Hvis du vil endre denne bokstaven til en annen form, må du bruke andre metoder, som for eksempel å bruke en switch case-funksjon.

Se også

- Vår artikkel om hvordan man bruker input til å styre en Arduino: https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/
- En guide til ASCII-kode og hvordan den fungerer: https://www.thejavaprogrammer.com/ascii-code/
- Arduino sin offisielle side med dokumentasjon og ressurser: https://www.arduino.cc/