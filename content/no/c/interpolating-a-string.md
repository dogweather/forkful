---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng i C-programmering er det å sette verdier inn i en streng ved kjøring. Dette gjør at programmene våre kan generere dynamisk og tilpasset tekst, noe som gir høyere fleksibilitet og brukervennlighet.

## Sånn gjør du:
La oss se på hvordan interpolere en streng i C ved hjelp av `sprintf` funksjonen.

```C
#include <stdio.h>

int main() {
    char buffer[50];
    int a = 10;
    float b = 20.5;

    sprintf(buffer, "Int: %d, Float: %f", a, b);
    printf("%s\n", buffer);

    return 0;
}
```

Dette vil gi følgende output:

```C
Int: 10, Float: 20.500000
```

## Dybdedykk
Historisk sett har C programmering ikke en innebygd strenginterpoleringsfunksjon som mange nyere språk. Men med bruk av funksjonene som `sprintf`, kan vi oppnå dette.

Alternativt kan `snprintf` brukes for å unngå buffer overflow, der andrepunktet er maksimal størrelse på strengen.

Når det gjelder implementeringsdetaljer, `sprintf` fungerer ved å formatere en streng og lagre den i buffervariabelen. `%d` og `%f` er format spesifiseringer for henholdsvis int og float.

## Se også 
For mer dyptgående detaljer og alternative metoder, se disse kildene:

* https://www.cplusplus.com/reference/cstdio/sprintf/
* https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm
* https://www.geeksforgeeks.org/snprintf-c-library/