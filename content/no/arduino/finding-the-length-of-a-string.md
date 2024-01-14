---
title:    "Arduino: Å finne lengden av en streng"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er nybegynner eller mer erfaren innenfor Arduino-programmering, vil du på et eller annet tidspunkt måtte håndtere strenger (engelsk: strings). Strenger refererer til en samling av tegn eller bokstaver som er satt sammen for å danne en tekst. Å finne lengden av en streng kan være nyttig når du arbeider med input fra brukere eller når du trenger å manipulere tekst i koden din.

## Hvordan gjøre det

For å finne lengden av en streng i Arduino, kan du bruke funksjonen `strlen()`, som finnes i standardbiblioteket `string.h`. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```
// Inkluderer den nødvendige headeren
#include <string.h>

void setup() {
    // Setter den serielle kommunikasjonshastigheten til 9600 baud
    Serial.begin(9600);
}

void loop() {
    // Oppretter en streng ved å bruke "String" datatypen
    String navn = "Ole Olsen";

    // Bruker strlen() for å finne lengden av strengen
    int lengde = strlen(navn);

    // Sender lengden av strengen til serieporten
    Serial.println(lengde);

    // Venter 2 sekunder før loopen kjører på nytt
    delay(2000);
}
```

I dette eksemplet oppretter vi først en streng og deretter bruker `strlen()` til å finne lengden av denne strengen. Resultatet blir deretter sendt til serieporten, slik at du kan se det i seriellmonitor. Husk å også inkludere `string.h` i koden din for å bruke `strlen()`.

## Dypdykk

Å finne lengden av en streng ved hjelp av `strlen()` er en veldig enkel og effektiv måte å håndtere tekst på i Arduino. Denne funksjonen går gjennom strengen og teller antall tegn før den når slutten av strengen, som ofte er markert med tegnet `\0`, også kjent som "nullterminering". Det betyr at `strlen()` stopper å telle når den kommer til dette tegnet.

Det er også verdt å nevne at `strlen()` fungerer bare på strenger som er laget ved hjelp av `String` datatypen. Dersom du bruker en C-streng (engelsk: C-string) som er lagret som en array av tegn i en variabel, vil du måtte bruke en annen metode for å finne lengden av strengen.

## Se også

- [Offisiell referanse for String akaratertegn (engelsk)](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Mer informasjon om C-strenger (engelsk)](https://www.programiz.com/c-programming/c-strings)
- [Andre nyttige funksjoner for å håndtere strenger i Arduino (engelsk)](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)