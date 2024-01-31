---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:04.020040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut felsökningsutdata innebär att visa tillfälliga meddelanden i konsolen för att förstå vad koden gör vid exekvering. Programmerare gör detta för att snabbt identifiera buggar och spåra variabler under utvecklingen.

## Hur gör man:
```C
#include <stdio.h>

int main() {
    int loopCounter = 0;
    for (int i = 0; i < 5; i++) {
        loopCounter++;
        printf("Loop iteration: %d\n", loopCounter);
        // Debug output to track the value of loopCounter
    }
    return 0;
}
```
Sample Output:
```
Loop iteration: 1
Loop iteration: 2
Loop iteration: 3
Loop iteration: 4
Loop iteration: 5
```

## Fördjupning:
Felsökningsutskrifter har använts sedan de tidigaste dagarna av programmering. Historiskt sett skulle datorer skriva ut resultat på papper. Idag använder vi konsolen eller logger. Alternativ till felsökningsutskrifter inkluderar användning av debuggers som GDB, instrumentering av kod, eller loggning med olika nivåer av detaljer. Viktiga detaljer vid implementering av felsökningsutskrifter innebär att välja relevanta meddelanden och att komma ihåg att ta bort eller dölja dem i produktionskoden.

## Se även:
- GDB Manual: https://sourceware.org/gdb/current/onlinedocs/gdb/
- Logging in C with syslog: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- C Programming Language (2nd Edition) by Brian W. Kernighan and Dennis M. Ritchie: ISBN 0-13-110362-8 (Provides foundational knowledge on C programming, including code tracing and debugging techniques).
