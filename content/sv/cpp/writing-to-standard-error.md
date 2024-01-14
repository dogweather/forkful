---
title:                "C++: Skriva till standardfel"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av C++ programmering, eftersom det ger utskrifter till standard error streamen som är viktiga för att ge information om fel i programmet. Detta kan hjälpa utvecklare att hitta och åtgärda problem snabbt och effektivt.

## Hur man gör

För att skriva till standard error streamen i C++, används funktionen `std::cerr`. Detta är en del av `iostream` biblioteket och behöver inte importeras separat. Här är ett enkelt exempel på hur man skulle skriva till standard error streamen:

```
#include <iostream>

int main(){

    int age = 25;
    
    std::cerr << "Min ålder är: " << age << std::endl;

    return 0;
}
```

Som du kan se i koden ovan, används `std::cerr` precis som `std::cout` för att skriva till standard output streamen. Skillnaden är att allt som skrivs till `std::cerr` skickas till standard error streamen istället för standard output streamen. Det är viktigt att märka att standard error streamen inte buffras på samma sätt som standard output streamen, vilket innebär att utskriften kommer att visas omedelbart.

När det gäller det faktiska användandet av `std::cerr`, kan den användas för att ge viktig information om programmet, som till exempel felmeddelanden och stack traces. Detta kan hjälpa utvecklare att hitta och åtgärda problem snabbt och effektivt.

## Djupdykning

När man skriver till standard error streamen i C++, är det viktigt att förstå hur detta fungerar för att kunna använda det effektivt. Som nämnts tidigare, buffras inte standard error streamen på samma sätt som standard output streamen. Detta innebär att texten som skrivs till standard error streamen kommer att visas direkt och i rätt ordning, även om den kombineras med utskrifter från standard output streamen.

Det är också viktigt att notera att `std::cerr` är baserad på `std::ostream`, vilket innebär att alla manipulatorer som fungerar för `std::cout` också fungerar för `std::cerr`. Detta inkluderar användning av `std::endl` för att avsluta utskriften och flytta markören till nästa rad.

En annan viktig aspekt att förstå är att standard error streamen är avsedd för att ge information om fel och inte för att användas för vanliga utskrifter. Detta betyder att det inte är lämpligt att använda `std::cerr` för att skriva ut till exempelvis användaren av programmet. För det syftet bör man istället använda `std::cout`.

## Se även (See Also)

- [Standard Library Headers in C++](https://www.learncpp.com/cpp-tutorial/1-12a-standard-library-headers/)
- [Debugging in C++](https://www.learncpp.com/cpp-tutorial/debugging-in-cpp/)
- [Output Streams in C++](https://www.geeksforgeeks.org/output-streams-in-c/)

Hoppas denna artikel har varit hjälpsam för att förstå varför och hur man skriver till standard error streamen i C++. Med rätt användning kan detta vara ett värdefullt verktyg för att hitta och åtgärda fel i program. Tack för att du läste!