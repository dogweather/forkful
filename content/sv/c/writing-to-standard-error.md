---
title:                "Skrivande till standardfel"
html_title:           "C: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

I C-programmering, är det vanligt att man skriver till standard error som en form av felsökning eller för att visa viktig information. Om du till exempel vill se felmeddelanden eller spåra hur ditt program körs, kan du använda standard error för att skriva ut detta data till terminalen.

## Hur man gör det

För att skriva till standard error i C, behöver du inkludera "stdio.h" biblioteket i ditt program. Du kan sedan använda "fprintf" funktionen för att skriva till standard error, istället för att använda "printf" som normalt skriver till standard output.

Här är ett enkelt exempel där vi skriver ett felmeddelande till standard error:

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Detta är ett felmeddelande.");
  return 0;
}
```

Kom ihåg att använda "stderr" i stället för "stdout" när du använder "fprintf".

När du kör detta program, kommer du att se att felmeddelandet skrivs ut i rött till terminalen. Detta kan vara användbart när du behöver markera fel eller annan viktig information för att tydligare se skillnaden mellan standard output och standard error.

## Djupdykning

Det finns många andra sätt att använda standard error i C-programmering. Du kan till exempel använda "perror" funktionen för att skriva ut det aktuella systemfelmeddelandet baserat på ett felkod som returnerats av ett systemanrop.

En annan användbar funktion är "strerror" som kan användas för att konvertera ett felkod till en sträng som beskriver felet.

Dessutom finns det en rad andra funktioner som kan användas för att formatera och skriva ut data till standard error, som "fprintf", "printf" och "sprintf".

## Se även

- [The fprint function in C on TutorialsPoint](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Error-Handling in C on GeeksforGeeks](https://www.geeksforgeeks.org/error-handling-c-programs/)
- [Unix Signals and Standard Error on StackOverflow](https://stackoverflow.com/questions/23474776/unix-signals-and-standard-error-stream)