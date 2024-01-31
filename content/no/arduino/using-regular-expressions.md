---
title:                "Bruk av regulære uttrykk"
date:                  2024-01-19
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk (regex) er mønstre brukt for å matche tegnsekvenser i tekst. Programmerere bruker det for å finne, erstatte eller validere data raskt og effektivt.

## Hvordan:
Arduino støtter ikke regulære uttrykk nativt, men du kan bruke String-funksjonene for enkel mønstermatching. Her er et eksempel:

```Arduino
String input = "Arduino123";
if (input.indexOf("Arduino") != -1) {
  Serial.println("Fant ordet 'Arduino'");
} else {
  Serial.println("Finner ikke ordet 'Arduino'");
}
```
Eksempeloutput:
```
Fant ordet 'Arduino'
```

For komplekse mønstre, må du bruke et ekstern bibliotek som `regex.h`. Legg dette til i koden ved å inkludere biblioteket og bruk det slik:

```Arduino
#include <regex.h>

void setup() {
  regex_t reg;
  const char * str = "Arduino123";
  const char * pattern = "^[A-Za-z]+\\d+$";

  regcomp(&reg, pattern, REG_EXTENDED);
  if (regexec(&reg, str, 0, NULL, 0) == 0) {
    Serial.println("Mønster funnet");
  } else {
    Serial.println("Mønster ikke funnet");
  }
  
  regfree(&reg);
}

void loop() {
  
}
```
Eksempeloutput:
```
Mønster funnet
```

**Merk:** Eksempler over krever at du har konfigurert Serial.begin() i setup-funksjonen.

## Dykk dypere:
Regulære uttrykk har vært et kraftig verktøy siden 1950-tallet, utviklet fra teoretisk arbeid av Stephen Kleene. I moderne programmering er det hovedsakelig innebygd i alle programmeringsspråk, selv om det varierer i implementasjon og effektivitet. På Arduino kan du bruke String-klassen for enkle oppgaver, men for komplekse regex-operasjoner må du ty til eksterne biblioteker som `regex.h`. Dette gir ikke like rik funksjonalitet som i språk som Perl eller Python, men kan hjelpe når mønstermatching er nødvendig.

## Se også:
- Arduino String Class Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- regex.h Library Documentation: https://github.com/nickgammon/Regexp
- "Mastering Regular Expressions" av Jeffrey Friedl for dypere forståelse om regex.
