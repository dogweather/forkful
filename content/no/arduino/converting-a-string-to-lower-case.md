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

# Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle bokstavene i strengen til deres tilsvarende små bokstaver. Dette er nyttig fordi det gjør det lettere å sammenligne strenger og søke etter bestemte tegn eller ord. Programmere gjør dette for å gjøre koden mer effektiv og lesbart.

# Hvordan:
```Arduino
String tekst = "HEI, JEG ER EN TEKST";
tekst.toLowerCase();
Serial.println(tekst);
```
```Arduino
String tekst = "Bygge roboten skrevNE store";
tekst.toLowerCase();
Serial.println(tekst);
```
Output:
```
hei, jeg er en tekst
bygge roboten skrevne store
```

# Dypdykk:
Å konvertere en streng til små bokstaver har vært en vanlig praksis i programmering siden de tidlige dagene av datamaskiner. Det ble vanligvis gjort for å redusere kompleksiteten og størrelsen på koden. Alternativene til å bruke innebygde funksjoner som `toLowerCase()` i Arduino inkluderer å skrive en egen funksjon eller å bruke biblioteker som stringfuncs.h. For å implementere dette i koden kan en enkel `for`-løkke brukes til å iterere gjennom strengen og endre bokstavene.

# Se også:
- [String LowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/lowercase/) - Dokumentasjon for `toLowerCase()` funksjonen i Arduino.
- [StringFuncs](https://github.com/starkillerOG/stringfuncs) - Et bibliotek for å manipulere strenger i Arduino.