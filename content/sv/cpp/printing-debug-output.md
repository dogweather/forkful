---
title:                "C++: Utskrift av felrapportering"
simple_title:         "Utskrift av felrapportering"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är ofta en ganska komplex process som kan innehålla många olika steg och variabler. När du utvecklar ett program är det viktigt att du kan spåra och förstå vad som händer i koden för att kunna lösa problem och optimera prestanda. Ett sätt att göra detta är genom att använda debug output, eller utskrifter av information från programmet som hjälper dig att förstå vad som händer vid olika punkter i koden. I denna bloggpost kommer vi att utforska varför och hur du kan använda debug output i dina C++-program.

## Så här gör du

För att börja använda debug output i dina C++-program behöver du först inkludera <iostream> biblioteket i din kod. Detta gör att du kan skriva ut text till konsolen eller terminalen. För att skriva ut en enkel sträng kan du använda "std::cout" tillsammans med utskriftsoperatören "<<" som i exemplet nedan.

```C++
#include <iostream>

int main(){
  std::cout << "Hej världen!" << std::endl;
  return 0;
}
```

Output:
```
Hej världen!
```

Du kan också skriva ut variabler genom att använda variabelnamnen i "<<" operatorn.

```C++
#include <iostream>

int main(){
  int x = 5;
  std::cout << "värdet av x är: " << x << std::endl;
  return 0;
}
```

Output:
```
värdet av x är: 5
```

## Djupdykning

Debug output kan vara väldigt användbart när du behöver förstå vad som händer i ditt program vid olika tidpunkter. Genom att skriva ut variabler, funktionernas resultat och andra viktiga punkter i koden, kan du enkelt följa koden och se om det händer några oönskade förändringar eller felaktiga värden. Detta kan hjälpa dig att hitta och lösa buggar och optimera din kod för bättre prestanda.

Det finns också andra tekniker som du kan använda för att utforma mer avancerad debug output, såsom att använda loggningsbibliotek eller implementera din egen debug output-funktion. Detta kan vara särskilt användbart när du jobbar med större och mer komplexa projekt.

## Se även

- [C++ Reference](https://www.cplusplus.com/reference/)
- [Debugging in C++](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [Logging and Debugging in C++](https://www.geeksforgeeks.org/logging-debugging-c-program/)