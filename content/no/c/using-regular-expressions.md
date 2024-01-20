---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk (regex) lar deg søke og manipulere tekst ved å definere mønstre. Programmerere bruker det for effektivitet og fleksibilitet i tekstbehandling, som validering, søking og tekstsubstitusjon.

## Hvordan gjøre det:
I C, bruk biblioteket `<regex.h>` for regex-funksjoner. Her er en enkel eksempelkoden:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;
    // Kompilerer regex for å finne ordet 'norsk'
    ret = regcomp(&regex, "norsk", 0);
    if (ret) {
        fprintf(stderr, "Kunne ikke kompilere regex\n");
        return 1;
    }
    // Matcher teksten
    ret = regexec(&regex, "Jeg elsker norsk mat!", 0, NULL, 0);
    if (!ret) {
        printf("Fant et treff!\n");
    } else if (ret == REG_NOMATCH) {
        printf("Ingen treff.\n");
    } else {
        fprintf(stderr, "Regex match feilet\n");
    }
    // Frigjør minnet som regexen bruker
    regfree(&regex);
    return 0;
}
```

Kjører du koden over, vil output være `Fant et treff!`, siden 'norsk' fins i teksten.

## Dykk dypere:
Regex har røtter tilbake til 1950-tallet og er essensielt i scripting og programmering. Alternativer som `strstr()` i C er enklere men mindre kraftfulle. Regex-biblioteket i C støtter POSIX regex standarder, men kan variere mellom systemer i ytelse og funksjoner.

## Se også:
- Online regex tester: https://regex101.com/
- POSIX regex dokumentasjon: https://pubs.opengroup.org/onlinepubs/009696799/basedefs/xbd_chap09.html
- GNU C Library regex dokumentasjon: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html