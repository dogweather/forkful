---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Sammenslåing av strenger (eller "string concatenation") er prosessen med å sette sammen to eller flere strenger til en. Som programmerere gjør vi dette for å manipulere og formatere tekstdata på en effektiv måte.

## Hvordan gjør du:

I C, bruker vi ofte `strcat()` funksjonen fra standardbiblioteket for å legge sammen strenger. Se hvordan det fungerer:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char setning[100] = "Hei, ";
    strcat(setning, "verden!");
    printf("%s\n", setning);
    return 0;
}
```

Denne koden vil gi følgende utdata:

```
Hei, verden!
```

## Dypere dykk:
Historisk sett har strengkonkatenering alltid vært grunnleggende for tekstbehandling i programmering. Imidlertid har håndteringen av det variert mellom forskjellige språk. C-standarden leverer `strcat()`, men det krever et destinasjonsbuffer som er tilstrekkelig stor til å holde begge strenger, ellers er overflyt en risiko.

Alternativt kan `strncat()` brukes for å sikre at ikke mer enn `n` tegn blir slått sammen for å forhindre bufferoverflyt:

```C
char setning[10] = "Hei, ";
strncat(setning, "verden!", 5);
printf("%s\n", setning);
```

Utgangen her vil være "Hei, verd".

## Se også:

For mer informasjon om sammenkobling av strenger i C, se disse nyttige ressursene:

1. strcat() - [https://www.cplusplus.com/reference/cstring/strcat/](https://www.cplusplus.com/reference/cstring/strcat/)
2. strncat() - [https://www.cplusplus.com/reference/cstring/strncat/](https://www.cplusplus.com/reference/cstring/strncat/)
3. Teksthåndtering i C - [https://www.learn-c.org/en/Text_strings](https://www.learn-c.org/en/Text_strings)