---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är sökmönster för att hitta eller ersätta text. Programmerare använder dem för att effektivisera textbearbetning och dataextraktion.

## Hur gör man:
```Arduino
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // wait for serial port to connect

  regex_t reg;
  const char * str = "Hej123Värld";
  // Skapar ett reguljärt uttryck för att hitta sekvenser av siffror
  if (regcomp(&reg, "[0-9]+", REG_EXTENDED) == 0) {
    regmatch_t matches[1];
    // Sök efter matchning
    if (regexec(&reg, str, 1, matches, 0) == 0) {
      int start = matches[0].rm_so;
      int end = matches[0].rm_eo;
      for(int i = start; i < end; i++) {
        Serial.print(str[i]);
      }
    }
  }
  regfree(&reg);  // Frigör minnet
}

void loop() {
  // Inga upprepande handlingar här.
}
```
Output:
```
123
```

## Djupdykning
Reguljära uttryck har sitt ursprung i matematisk teori från 1950-talet. Alternativ till Arduino's `regex.h` bibliotek är att göra manuell strängbearbetning eller använda inbyggda funktioner som `String.indexOf()` och `String.substring()`. Implementering av regex i Arduino är begränsad jämfört med PC-datorer på grund av begränsad minne och processorkraft. Användningar bör vara enkla och resurssnåla.

## Se även
- Arduino's String klassreferens: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- En introduktion till reguljära uttryck: https://www.regular-expressions.info/
- Uppslagsguide för reguljära uttryck i C++: http://www.cplusplus.com/reference/regex/
