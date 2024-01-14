---
title:                "Arduino: Å finne lengden av en streng"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen være interessert i å finne lengden av en streng? Vel, det kan være en nyttig ferdighet å ha når man jobber med Arduino-programmering. Å kunne bestemme lengden på en streng kan være nyttig for å håndtere input og output data, og generelt gjøre koden din mer effektiv.

## Slik gjør du det
For å finne lengden av en streng i Arduino, kan du bruke funksjonen `strlen()`. Denne funksjonen tar inn en streng som parameter og returnerer lengden av strengen som et heltall. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Arduino
char tekst[] = "Hello World";
int lengde = strlen(tekst);
Serial.println(lengde);
```

I dette eksempelet definerer vi en streng med variabelnavnet `tekst` og tilordner den verdien "Hello World". Deretter bruker vi `strlen()`-funksjonen til å finne lengden av denne strengen og lagrer den i en variabel med navnet `lengde`. Til slutt skriver vi ut lengden til Serial Monitor ved hjelp av `Serial.println()`.

Resultatet av dette eksempelet vil være 11, siden "Hello World" består av 11 tegn.

## Dykk dypere
Det er viktig å forstå at `strlen()`-funksjonen teller antall tegn i en streng, selv om noen av tegnene kan være spesielle tegn som ikke vises når de skrives. For eksempel, hvis du har en streng med verdien "Hello\n", vil `strlen()` returnere 6 fordi \n tilsvarer ett tegn.

Det er også verdt å merke seg at `strlen()`-funksjonen ikke inkluderer \0 (null terminator) i lengden av en streng. Dette er en spesiell karakter som brukes til å markere slutten av en streng i C og C++ programmering.

## Se også
- [Arduino - String Text Length](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [C++ String Length Tutorial](https://www.programiz.com/cpp-programming/library-function/cstring/strlen)
- [GeeksforGeeks - Finding Length of a String in C++](https://www.geeksforgeeks.org/how-to-find-length-of-a-string-in-cpp/)

Håper dette har vært nyttig for å forstå hvordan man kan finne lengden av en streng i Arduino-programmering. Lykke til med din neste kode!