---
title:                "Att använda en debugger"
date:                  2024-01-26T03:48:17.601076-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En debugger är ett verktyg som låter dig inspektera din C-kod medan den körs, steg för steg, för att spåra buggar. Programmerare använder debuggers för att förstå hur deras kod beter sig, fixa problem och optimera prestanda utan att gissa.

## Hur gör man:
Säg att du arbetar med ett enkelt C-program som beräknar fakulteten av ett nummer, men det finns ett fel. För att använda en debugger som `gdb` (GNU Debugger), kompilera först med flaggan `-g` för att inkludera debug-information:

```c
// kompilera med: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // En enkel kontroll för negativ inmatning
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("Fakulteten av %d är %ld\n", number, result);
    return 0;
}
```

Kör sedan det i gdb:

```shell
$ gdb ./factorial
```

Ställ in en brytpunkt vid funktionen `factorial` och kör programmet:

```gdb
(gdb) break factorial
(gdb) run
```

När den träffar brytpunkten, stega igenom varje rad med `next` eller `n` och inspektera variabler med `print` eller `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Exempelutskrift kommer att ge realtidsvärden och informationsflöde om programmets exekvering.

## Djupdykning
Debuggers har funnits sedan 1960-talet, och har utvecklats från enkla monitorer till komplexa applikationer med grafiska användargränssnitt. Traditionell utskriftsbaserad felsökning var vanlig innan mogna debuggers utvecklades. Alternativ till `gdb` inkluderar `lldb`, `dbx` eller IDE-integrerade debuggers som de i Visual Studio eller CLion.

När man hanterar debuggers varierar implementeringen—vissa kan fånga körtidsfel, undersöka minnet eller till och med omvända programkörningen. `gdb` kan fästa vid körande processer, vilket möjliggör felsökning av redan körande mjukvara, en fördel för att åtgärda live systemfel.

## Se även
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Felsökningstekniker i C: http://www.cprogramming.com/debugging/debugging.html
