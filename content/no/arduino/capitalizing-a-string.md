---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
simple_title:         "Sette streng til store bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sette store bokstaver i en streng betyr å endre alle bokstavene til versaler (store bokstaver). Programmerere gjør dette for å standardisere tekstdata, for eksempel for å vise navn og titler konsekvent.

## Slik gjør du:
Husk at Arduino ikke har innebygd strengbehandling som andre programmeringsspråk. Vi må selv lage en funksjon for dette:

```arduino
void setup() {
  Serial.begin(9600);
  char tekst[] = "Arduino er gøy!";
  kapitaliserStreng(tekst);
  Serial.println(tekst);
}

void loop() {
  // Ingenting her
}

void kapitaliserStreng(char* linje) {
  for (int i = 0; linje[i] != '\0'; i++) {
    linje[i] = toupper(linje[i]);
  }
}
```

Kjører du koden, ser du dette i Serial Monitor:
```
ARDUINO ER GØY!
```

## Dypdykk
Historisk sett kommer behovet for å kapitalisere fra tidlige datamaskiner og brukergrensesnitt som hadde begrenset grafikk, der store bokstaver skilte seg mer ut. Et alternativ til vår `kapitaliserStreng`-funksjon er å bruke String-objekter som inkluderer `.toUpperCase()`-metoden, men disse kan føre til minnelekkasje på minnebegrensede systemer som Arduinoer. Detaljert, streng-kapitalisering i Arduino krever manuell iterasjon over hver karakter i et array og å bruke `toupper()` funksjonen fra `ctype.h`-biblioteket.

## Se Også
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ASCII Table and Description: http://www.asciitable.com/
- `ctype.h` library documentation: https://www.cplusplus.com/reference/cctype/
