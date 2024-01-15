---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en streng til små bokstaver er en vanlig oppgave i programmering. Det kan være nyttig for å sammenligne strenger eller hvis man ønsker å lage en enhetlig utskrift. 

## Slik gjør du det

Arduino har en innebygd funksjon, `toLowerCase()`, som gjør nettopp dette. La oss se på et eksempel:

```Arduino
String tekst = "HEI DER!";
tekst = tekst.toLowerCase();
Serial.print(tekst); // vil skrive ut "hei der!"
```

Som du kan se, konverterte `toLowerCase()` funksjonen `tekst`-strengen vår til små bokstaver. Dette er en enkel og effektiv måte å konvertere en streng på. 

## Dypdykk

Når det gjelder å konvertere en streng til små bokstaver, er det viktig å være oppmerksom på at siden Arduino er et C++ basert språk, vil det ta hensyn til forskjeller i store og små bokstaver. Dette vil si at "A" og "a" blir behandlet som forskjellige bokstaver.

Det finnes også andre måter å konvertere en streng til små bokstaver på, som for eksempel å bruke en løkke for å gå gjennom hver enkelt bokstav og endre den til en små bokstav ved hjelp av ASCII-kode konvertering. Men dette kan være mer komplisert og krever en grundigere forståelse av programmering. 

## Se også
- [Arduino String Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C++ tolower() Documentation](https://www.cplusplus.com/reference/cctype/tolower/)
- [ASCII Table](https://www.asciitable.com/)