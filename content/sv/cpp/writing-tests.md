---
title:                "Skriva tester"
html_title:           "C++: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man bör inkludera tester i sin C++-kod. För det första, det kan hjälpa till att upptäcka och förhindra buggar i koden. Det är också ett praktiskt sätt att säkerställa att koden fungerar som den ska och fortsätter göra det även efter eventuella framtida ändringar.

## Så här gör du

Att skriva tester i C++ är inte så svårt som det kanske verkar. Det finns flera olika ramverk som underlättar processen, som till exempel Google Test och Catch2. Nedan följer ett enkelt exempel på hur du kan skriva ett test för en funktion som adderar två tal:

```C++
#include <iostream>

// Funktionen som ska testas
int add(int a, int b) {
    return a + b;
}

int main() {
    // En enkel testcase
    int result = add(2, 3);

    // Om resultatet är rätt så skrivs "Test passed!" ut
    // Annars skrivs "Test failed!" ut
    if (result == 5) {
        std::cout << "Test passed!" << std::endl;
    } else {
        std::cout << "Test failed!" << std::endl;
    }

    return 0;
}
```

Kom ihåg att testa både positiva och negativa scenarier för att vara säker på att koden fungerar som den ska.

## Djupdykning

När du skriver tester i C++ är det viktigt att förstå hur testbaserade klasser fungerar. En testbaserad klass är en klass som innehåller en uppsättning tester för en funktionell enhet, som till exempel en klass eller en funktion. Genom att använda dessa klasser kan du enkelt organisera och köra dina tester, vilket gör det enklare att upptäcka och åtgärda buggar.

Du bör också överväga att använda en CI (continuous integration) plattform för att automatiskt köra dina tester varje gång du gör en förändring i koden. Detta hjälper till att identifiera eventuella problem tidigt och säkerställer att koden alltid är i ett fungerande skick.

## Se även

- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)
- [Continuous Integration](https://medium.com/@onejohi/continuous-integration-vs-continuous-delivery-vs-continuous-deployment-5c3e869d4e65)