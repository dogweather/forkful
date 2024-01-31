---
title:                "Omvandla ett datum till en sträng"
date:                  2024-01-20T17:35:53.909798-07:00
model:                 gpt-4-1106-preview
simple_title:         "Omvandla ett datum till en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att konvertera ett datum till en sträng innebär att du omvandlar datumdata från ett format som datorn förstår till något lättläst för människor. Programmerare gör detta för att visa datum på skärmar, i rapporter eller för att spara i textbaserade format som CSV.

## How to:
Använd `strftime` för att formatera datum- och tidsdata som en sträng.

```C
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Hämta nuvarande tid och datum
    time(&rawtime);
    timeinfo = localtime(&rawtime);

    // Formatera och konvertera till sträng
    strftime(buffer, sizeof(buffer), "%Y-%m-%d %H:%M:%S", timeinfo);
    printf("Nuvarande datum och tid: %s\n", buffer);

    return 0;
}
```
### Exempelutdata:
```
Nuvarande datum och tid: 2023-04-01 17:45:31
```

## Deep Dive:
Innan `strftime` var standard, användes funktioner som `sprintf` och anpassad logik för att formatera datum. Alternativ finns även i form av externa bibliotek som `date.h` i C++20 eller olika tidsbibliotek i andra språk. Implementeringsdetaljer som tidszoner och lokaliseringsinställningar kan påverka hur `strftime` fungerar, så det är viktigt att definiera dessa korrekt för att få önskat utformat datum och tid.

## See Also:
- C Standard Library - `strftime` funktion: https://en.cppreference.com/w/c/chrono/strftime
- GNU C Library Reference Manual: https://www.gnu.org/software/libc/manual/ 
- C++ date.h bibliotek för modern tidsbehandling: https://en.cppreference.com/w/cpp/chrono
