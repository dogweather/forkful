---
title:    "Arduino: Å finne lengden på en streng"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en vanlig oppgave i programmering, spesielt hvis man jobber med dataanalyse, tekstbehandling eller kommunikasjon med eksterne enheter. Det er også en grunnleggende kunnskap som er viktig å mestre når man lærer å programmere.

## Hvordan

For å finne lengden på en streng i Arduino, kan vi bruke funksjonen `strlen()`. Denne funksjonen tar inn en streng som parameter og returnerer lengden på strengen. La oss se på et eksempel:

```Arduino
String name = "Erik";
int length = strlen(name);
Serial.println(length); // Dette vil skrive ut tallet 4, da "Erik" har 4 bokstaver
```

Som du kan se, er det veldig enkelt å finne lengden på en streng. Man kan også bruke `name.length()` som er en annen innebygd funksjon i Arduino for å få samme resultat.

## Dypdykk

En ting å være oppmerksom på er at `strlen()` fungerer på en måte som er forskjellig fra de fleste andre språk. Hvis du prøver å bruke den på en variabel som ikke er en streng, vil den gi uforutsigbare resultater. I stedet kan du bruke funksjonen `sizeof()`, som vil gi deg størrelsen på variabelen i bytes. Dette er spesielt nyttig hvis du jobber med arrays og trenger å finne størrelsen på dem.

En annen ting å merke seg er at `strlen()` teller bare bokstaver i en streng, ikke symboler som mellomrom eller tegn med aksenter. Hvis du vil inkludere disse i tellingen, kan du bruke `name.getBytes()` som vil gi deg et array med hver enkelt byte i strengen.

## Se også

* [Offisiell Arduino dokumentasjon for `strlen()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
* [Tutorial om strings i Arduino](https://www.arduino.cc/en/Tutorial/StringLengthTrim)
* [Stack Overflow diskusjon om å finne lengden på en streng i Arduino](https://stackoverflow.com/questions/18195450/how-to-find-the-length-of-a-string-in-arduino)