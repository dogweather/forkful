---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:34:58.533104-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av datoer fra strenger gjør tekstomvorming til strukturerte datatyper mulig, noe som er kritisk for datobehandling. Programmerere trenger dette for datalagring, sammenligning og manipulasjon.

## Hvordan:
For å parse en dato fra en streng i C, kan vi bruke `strptime`-funksjonen fra `<time.h>`-biblioteket. Her er et eksempel:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *str = "2023-04-05";
    
    if (strptime(str, "%Y-%m-%d", &tm) != NULL) {
        printf("År: %d, Måned: %d, Dag: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Feil format på dato\n");
    }
    
    return 0;
}
```

Eksempel på output:

```
År: 2023, Måned: 4, Dag: 5
```

## Dypdykk:
Tidligere brukte vi `sscanf` eller manuell inndatahåndtering for å parse datoer, noe som ikke alltid var robust. `strptime` gir en standardisert metode for å interpretere datoer og tider. Utfordringen er at `strptime` ikke er en del av C-standarden (C11), selv om den ofte er tilgjengelig på Unix-lignende systemer. Alternativer inkluderer strukturen `sscanf` eller tredjepartspakker som "date.h".

Detaljert, `strptime` leser inn en streng i henhold til et format og fyller en `struct tm` med informasjonen. Implementasjonen kan variere mellom systemer, så det er viktig å sjekke dokumentasjonen.

## Se også:
- C Standard Library Reference: https://en.cppreference.com/w/c/chrono
- GNU C Library Manual `strptime`: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html
- date.h – A date and time library: https://github.com/HowardHinnant/date