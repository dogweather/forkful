---
title:                "Utskrift av felinformation"
html_title:           "C++: Utskrift av felinformation"
simple_title:         "Utskrift av felinformation"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Det finns många gånger när en programmerare behöver en enkel och snabb metod för att diagnostisera och lösa problem i sin kod. Att använda utskrift av felsökningsmeddelanden kan vara en effektiv lösning för att förstå exakt vad som händer i programmet.

## Hur man gör
Det är lätt att lägga till utskrift av felsökningsmeddelanden i din C++-kod. Använd detta enkla kodblock som exempel:

```C++
#include <iostream>

int main() {
    
  // Utskrift av felsökningsmeddelande
  std::cout << "Det här är ett felsökningsmeddelande." << std::endl;
  
  return 0;
}
```

Koden ovan använder `std::cout` för att skriva ut meddelandet på skärmen och `std::endl` för att lägga till en radbrytning. Detta är den grundläggande syntaxen för att skriva ut felsökningsmeddelanden, men det finns många andra möjligheter att utforska, som att skriva ut variabler, objekt eller felmeddelanden.

En annan användbar teknik är att använda en conditional statement, som `if` eller `while` för att styra när utskriften ska ske. Till exempel:

```C++
#include <iostream>

int main() {
    
  int x = 5;
  int y = 10;
  
  // Skriv endast ut om x är större än y
  if (x > y) {
    std::cout << "x är större än y." << std::endl;
  }
  
  return 0;
}
```

Detta kan vara särskilt användbart när du försöker hitta fel eller buggar i din kod, eftersom det ger dig möjlighet att enkelt kontrollera eller jämföra variabler vid olika punkter i ditt program.

## Djupdykning
Att lägga till felsökningsutskrifter i din kod kan även vara användbart för att förstå hur din kod fungerar på en djupare nivå. Genom att skriva ut variabler och objekt vid olika steg i programmet kan du få en tydligare bild av hur värdena ändras och påverkar varandra.

Dessutom kan du använda felsökningsutskrifter för att hitta och åtgärda fel i din kod. Genom att skriva ut felmeddelanden eller använda conditional statements kan du lättare lokalisera och lösa problem i din kod.

En sak att tänka på är att ta bort eller inaktivera dina felsökningsutskrifter när du är färdig med att testa och felsöka din kod. Att ha överflödiga utskrifter i din slutliga version av programmet kan påverka dess prestanda.

## Se även
- [Debugging in C++](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [Using cout for Debugging in C++](https://www.geeksforgeeks.org/using-cout-debugging-c/)