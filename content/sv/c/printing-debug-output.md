---
title:    "C: Utskrift av felsökningsutdata"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en viktig del av programmering, men ibland kan det vara svårt att veta vad som händer bakom kulisserna. Det är här debugging kommer in i bilden. Genom att skriva ut debug output kan du enkelt se vad som händer i din kod och på så sätt hitta och korrigera eventuella felaktigheter eller buggar. I denna bloggpost kommer vi att titta närmare på hur du kan skriva debug output i C-programmering.

## Hur man gör

För att skriva debug output i ditt C-program behöver du först inkludera standardbiblioteket "stdio.h". Du kan sedan använda funktionen "printf()" för att skriva ut önskad information. Här är ett enkelt exempel:

```C
#include <stdio.h>

int main(){
    int x = 5; //En variabel för att testa debug output
    printf("Variabelns värde är %d\n", x);
    return 0;
}
```

I detta exempel använder vi "%d" för att skriva ut värdet av variabeln "x". Du kan också använda andra formatteringsalternativ beroende på typen av variabel du vill skriva ut. Till exempel "%s" för strängar eller "%f" för flyttal.

En annan användbar funktion för debug output är "fprintf()". Den fungerar på samma sätt som "printf()" men låter dig skriva ut information till en specifik fil istället för standardutmatningen. Detta kan vara användbart för att logga viktig information under körning av ditt program.

## Djupdykning

Att skriva debug output är en viktig del av att utveckla C-program. Det hjälper dig att förstå vad som händer i ditt program och hitta eventuella fel eller buggar. Det finns dock några saker att tänka på när du skriver debug output.

För det första bör du undvika att skriva ut för mycket information. Det kan bli överväldigande och göra det svårt att hitta viktig information. Välj istället noga ut vad som är relevant för det specifika problem du försöker lösa.

För det andra bör du ta bort eller kommentera ut dina debug statements när ditt program är färdigt och fungerar som det ska. Att behålla dem kan leda till onödig overhead och göra koden svårare att läsa.

## Se även

- 10 Essential Debugging Tips for C Programmers (https://www.linuxjournal.com/article/2584)
- Using Debug Statements for Detecting Errors in C Programs (https://www.geeksforgeeks.org/using-debug-statements-for-detecting-errors-in-c-programs/)
- How to Use Debug Statements Effectively (https://www.thoughtco.com/print-error-statements-2908485)