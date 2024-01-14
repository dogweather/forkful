---
title:    "C++: Skriva till standardfel"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error är ett viktigt verktyg för att förstå och felsöka program i C++. Genom att skriva till standard error kan du utskriva meddelanden och felmeddelanden som hjälper dig att förstå vad som händer i ditt program.

## Hur man gör det
För att skriva till standard error i C++ behöver du inkludera "iostream" biblioteket och använda "cerr" funktionen. Här är ett exempel på hur du kan göra det:

```C++
#include <iostream>

int main() {
    if (true) {
        std::cerr << "Ett fel inträffade!" << std::endl;
    }
    return 0;
}
```

I detta exempel skriver vi texten "Ett fel inträffade!" till standard error genom att använda "cerr" funktionen. Sedan använder vi "endl" för att lägga till en radbrytning för tydlighet. När programmet körs kommer texten att skrivas ut till standard error och programmet kommer att fortsätta köra som vanligt.

## Djupdykning
Att skriva till standard error är en del av det standarda utflödet (standard output) som är tillgängligt i de flesta programspråk. Det används främst för att skriva ut felmeddelanden eller annan information som är viktig för att förstå vad som händer i programmet. Ett vanligt fel är att blanda ihop "cerr" och "cout", vilket kan leda till förvirring och felaktiga utskrifter. En annan viktig sak att komma ihåg är att "cerr" inte buffrar utdata, vilket innebär att den skriver till standard error direkt utan att vänta på att programmet ska avslutas. Detta är särskilt användbart vid felsökning då du vill se felmeddelanden omedelbart istället för senare när programmet har kört klart.

## Se också
- [C++ Standard Library](https://en.cppreference.com/w/cpp/header)
- [Cerr Reference](https://en.cppreference.com/w/cpp/io/cerr)
- [Understanding Standard Error in C++](https://www.learncpp.com/cpp-tutorial/understanding-standard-iostream/)