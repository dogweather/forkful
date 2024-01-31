---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster som används för att matcha strängsekvenser. Programmerare använder dem för att söka, validera och manipulera text effektivt.

## How to:
I C använder vi biblioteket `<regex.h>` för reguljära uttryck. Se exemplet nedan:

```c
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex;
    int reti;
    char msgbuf[100];

    // Kompilera reguljärt uttryck
    reti = regcomp(&regex, "^a[[:alnum:]]", 0);
    if (reti) {
        fprintf(stderr, "Kunde inte kompilera uttryck\n");
        return 1;
    }

    // Utför matchning
    reti = regexec(&regex, "abc", 0, NULL, 0);
    if (!reti) {
        puts("Match funnen");
    }
    else if (reti == REG_NOMATCH) {
        puts("Ingen match funnen");
    }
    else {
        regerror(reti, &regex, msgbuf, sizeof(msgbuf));
        fprintf(stderr, "Matchningsfel: %s\n", msgbuf);
        return 1;
    }

    // Frigör allokerade resurser
    regfree(&regex);

    return 0;
}
```
Utdata: `Match funnen`

## Deep Dive
Reguljära uttryck härstammar från matematisk teori från 1950-talet. Alternativ i C är bibliotek som `PCRE` (Perl Compatible Regular Expressions) och utbyggda funktioner i språk som Python och Javascript. Implementeringen använder ofta tillståndsmaskiner och backtracking för att matcha mönster.

## See Also
- POSIX regex man page: [http://man7.org/linux/man-pages/man7/regex.7.html](http://man7.org/linux/man-pages/man7/regex.7.html)
- PCRE dokumentation: [https://www.pcre.org](https://www.pcre.org)
- Övning med reguljära uttryck: [https://regexr.com](https://regexr.com)
