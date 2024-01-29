---
title:                "Een debugger gebruiken"
date:                  2024-01-28T22:08:32.208523-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger is een hulpmiddel waarmee je je C-code kunt inspecteren terwijl deze wordt uitgevoerd, stap voor stap, om fouten op te sporen. Programmeurs gebruiken debuggers om te begrijpen hoe hun code zich gedraagt, problemen op te lossen en de prestaties te optimaliseren zonder te gissen.

## Hoe te gebruiken:
Stel je werkt met een eenvoudig C-programma dat de faculteit van een getal berekent, maar er is een storing. Om een debugger zoals `gdb` (GNU Debugger) te gebruiken, compileer je eerst met de `-g` vlag om debug-informatie in te sluiten:

```c
// compileer met: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Een eenvoudige controle op negatieve invoer
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("De faculteit van %d is %ld\n", number, result);
    return 0;
}
```

Voer het vervolgens uit in gdb:

```shell
$ gdb ./factorial
```

Stel een breekpunt in bij de `factorial` functie en voer het programma uit:

```gdb
(gdb) break factorial
(gdb) run
```

Wanneer het het breekpunt bereikt, loop je door elke regel met `next` of `n` en inspecteer je variabelen met `print` of `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

Voorbeelduitvoer zal realtime waarden en programma-uitvoeringsstroom bieden.

## Diepduiken
Debuggers bestaan sinds de jaren 60 en zijn geëvolueerd van eenvoudige monitoren tot complexe, op GUI gebaseerde applicaties. Traditioneel print-gebaseerd debuggen was gebruikelijk voordat volwassen debuggers werden ontwikkeld. Alternatieven voor `gdb` omvatten `lldb`, `dbx`, of IDE-geïntegreerde debuggers zoals die in Visual Studio of CLion.

Bij het omgaan met debuggers varieert de implementatie - sommige kunnen runtimefouten vangen, het geheugen onderzoeken of zelfs de uitvoering van een programma omkeren. `gdb` kan worden gekoppeld aan actieve processen, waardoor het debuggen van reeds lopende software mogelijk is, een zegen voor het oplossen van live systeemfouten.

## Zie Ook
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debuggen met GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Debugtechnieken in C: http://www.cprogramming.com/debugging/debugging.html
