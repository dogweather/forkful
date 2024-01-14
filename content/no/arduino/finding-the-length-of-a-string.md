---
title:                "Arduino: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en vanlig oppgave innen programmering. Det kan være nyttig når du jobber med tekst og må vite nøyaktig hvor mange tegn som er tilgjengelig. I denne bloggposten vil vi vise deg hvordan du enkelt kan finne lengden på en streng ved hjelp av Arduino.

## Hvordan

For å finne lengden på en streng i Arduino, kan du bruke funksjonen `strlen()`. Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen. La oss se på et eksempel:

```Arduino
String tekst = "Hei, dette er en tekststreng";
int lengde = strlen(tekst);
Serial.println(lengde); //output: 29
```

Her har vi definert en variabel `tekst` som inneholder en tekststreng, og så har vi brukt `strlen()` til å finne lengden på strengen og lagret den i variabelen `lengde`. Deretter skriver vi ut lengden ved hjelp av `Serial.println()`.

Du kan også bruke `strlen()` til å finne lengden på en streng som er lagret i en array. For eksempel:

```Arduino
char tekst[] = "Dette er en tekststreng";
int lengde = strlen(tekst);
Serial.println(lengde); //output: 24
```

Her har vi brukt en `char` array i stedet for en `String` og funksjonen `strlen()` fungerer fortsatt på samme måte.

## Deep Dive

Hvis du vil gå dypere inn i hvordan `strlen()` fungerer, kan du se på koden for funksjonen. Denne funksjonen er faktisk en del av standard C bibliotek, så den er ikke spesifikk for Arduino. Du kan finne kildekoden for `strlen()` i [string.h](https://github.com/arduino/ArduinoCore-avr/blob/master/avr-libc/string.h) filen.

I hovedsak går funksjonen gjennom hver tegn i strengen og teller antall tegn til den når slutten av strengen. Den returnerer så antall tegn som telleren har lagret.

## Se også

- [String lengde funksjonen referanse](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [C string bibliotek](https://www.cplusplus.com/reference/cstring/)