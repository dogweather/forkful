---
title:                "Å bruke regulære uttrykk"
html_title:           "Arduino: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Regulære uttrykk brukes til å søke etter spesifikke mønstre i en streng. Det brukes av programmerere for data validering, søk & erstatt og streng redigering.

## Hvordan:

La oss lage et enkelt C-program som bruker regex. Det inkluderer `'regex.h'` headerfilen og bruker `'regcomp()` og `'regexec()'` funksjoner for å matche regulære uttrykk.

```C
#include <regex.h> 
#include <stdio.h>

int main() {
    regex_t regex;
    int ret;
    ret = regcomp(&regex, "[a-z]", 0);
    if (ret) {
        printf("Kunne ikke kompilere regex\n");
        return(1);
    }
    ret = regexec(&regex, "abc", 0, NULL, 0);
    if (!ret) {
        printf("Match funnet\n");
    } else if (ret == REG_NOMATCH) {
        printf("Ingen match\n");
    } else {
        printf("Regex match feilet\n");
    }
    regfree(&regex);
    return 0;
}
```

I denne koden kompilerer 'regcomp()' regulært uttrykk i sin interne form, og 'regexec()' sjekker om tekststrengen matcher det kompilerte regulære uttrykket.

## Dypdykk:

Bruk av regulære uttrykk har røtter i teoretisk informatikk og formalisering av syntaksanalyse. 

Alternativer til bruk av regulære uttrykk inkluderer spørringsbaserte språk som SQL og XPath, og kodespesifikke søkeverktøy.

Det er også lite kjente implementeringsdetaljer. For eksempel kan kompilering av et regulært uttrykk resultere i ulike interne former, avhengig av C-bibliotekets implementasjon.

## Se også:

For mer dyptgående detaljer om regulære uttrykk i C, sjekk ut følgende ressurser:
- POSIX regulære uttrykk: http://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html 
- Regulære Uttrykk Tutorial: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html 
- Regulære Uttrykk med eksempler: https://www.geekhideout.com/regex.html