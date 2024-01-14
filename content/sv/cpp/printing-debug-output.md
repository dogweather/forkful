---
title:                "C++: Utskrift av felsökningsutdata"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

När du utvecklar program kan det vara användbart att ha utskrift av felsökning för att hålla koll på vad som händer i ditt program och för att enkelt hitta och åtgärda eventuella problem.

## Hur man gör det

För att skriva ut felsökningsutmatning i C++ använder vi funktionen `cout` från standard biblioteket `iostream`. Vi behöver även inkludera detta bibliotek i början av vårt program med hjälp av `#include <iostream>`. Här är ett exempel på hur man skriver ut en sträng:

```C++
#include <iostream>

int main() {
    std::cout << "Hej världen!" << std:endl;
    return 0;
}
```

Detta kommer att skriva ut "Hej världen!" på skärmen. Vi kan även skriva ut variabler genom att inkludera dem i `cout`:

```C++
#include <iostream>

int main() {
    int tal = 100;
    std::cout << "Mitt tal är " << tal << std::endl;
    return 0;
}
```

Detta kommer att skriva ut "Mitt tal är 100" på skärmen.

## En djupdykning

Att inkludera felsökningsutmatning i ditt program kan hjälpa dig att förstå vad som händer i ditt program och hitta eventuella fel. Det är en enkel och effektiv metod för felsökning och kan spara mycket tid i det långa loppet. Det är viktigt att komma ihåg att ta bort alla felsökningsutmatning när ditt program är färdigt för att undvika onödig kod och förbättra prestandan.

## Se även

- [C++ - Standard bibliotek](https://www.cplusplus.com/reference/)
- [Felsökningsutmatning i C++](https://www.learncpp.com/cpp-tutorial/15-introduction-to-debugging/)