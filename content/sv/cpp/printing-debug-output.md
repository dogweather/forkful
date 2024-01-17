---
title:                "Utskrift av felsökningsresultat"
html_title:           "C++: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut felsöknings-output är en viktig del av programmering. Det innebär att skriva ut information under körning av programmet för att se vad som händer och hjälpa till att lösa problem. Program som innehåller många buggar kan vara svåra att felsöka, och genom att skriva ut debug output kan man få en bättre förståelse för vad som orsakar problemen.

## Hur man:

```C++
// Kod exempel
#include <iostream>

int main() {
  int i = 5;
  std::cout << "Värdet av i: " << i << std::endl;
  return 0;
}
```

Output:
```
Värdet av i: 5
```

För att skriva ut debug output i C++, använder man sig av std::cout funktionen, som finns inbyggd i C++ biblioteket. Genom att inkludera <iostream> så kan man använda std::cout för att skriva ut information till konsolen. Det är viktigt att inkludera slutligen "std::endl" för att skriva ut en ny rad och "return 0" för att avsluta programmet.

## Djupdykning:

Att skriva ut felsöknings-output har varit en viktig del av programmering sedan tidigt 1970-tal, då C-programmeringsspråket utvecklades. Det har blivit standard för många moderna programmeringsspråk, och används fortfarande i dag för att hjälpa till att lösa problem.

Ett alternativ till att skriva ut debug output är att använda en debugger, som är ett verktyg som hjälper till att hitta och lösa problem i programmet. Det kan vara svårt att hitta fel utan att skriva ut debug output, men en kombination av båda metoderna kan vara användbar.

För att implementera debug output i ett program, är det viktigt att använda sig av lämpliga datatyper för att skriva ut informationen. Annars kan det leda till problem, som exempelvis att skriva ut en pekare istället för det önskade värdet av en variabel. Det är också viktigt att balansera användningen av debug output, eftersom för mycket utskrifter kan påverka programmet och göra det långsammare än nödvändigt.

## Se även:

- [C++ - std::cout docs](https://www.cplusplus.com/reference/iostream/cout/)
- [Debugging in C++ - A Beginner's Guide](https://www.geeksforgeeks.org/debugging-c-set-1-simple-print-macros/#basic-macro)