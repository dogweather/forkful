---
title:                "C++: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att veta den aktuella datumet kan vara mycket användbart i olika typer av program, till exempel bokföring eller kalendrar. Genom att använda C++ kan du på ett enkelt sätt få tillgång till den aktuella datumet och sedan använda den i ditt program.

## Hur man gör

Det finns flera olika sätt att få tillgång till den aktuella datumet i C++. Ett sätt är att använda funktionen `time()` som finns i standardbiblioteket `<ctime>`. Här är ett enkelt exempel på hur du kan använda denna funktion för att få den aktuella datumet:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Skapa en variabel som lagrar tiden i sekunder sedan 1 januari 1970
    time_t tiden = time(0);

    // Skapa en struktur för att lagra det aktuella datumet
    tm* nu = localtime(&tiden);

    // Skriv ut datumet i formatet år-månad-dag
    std::cout << nu->tm_year + 1900 << "-" << nu->tm_mon + 1 << "-" << nu->tm_mday << std::endl;

    return 0;
}
```

Detta program kommer att ge följande output:

```
2021-04-17
```

Det är viktigt att notera att `time()` funktionen returnerar tiden i sekunder sedan 1 januari 1970. Därför måste vi använda funktionen `localtime()` för att konvertera tiden till en förståelig struktur där vi kan få tillgång till datumet.

## Djupdykning

För de som är intresserade av att lära sig mer om hur man får tillgång till den aktuella datumet i C++, finns det flera andra bibliotek och funktioner som kan användas. Till exempel finns det `<chrono>` biblioteket som erbjuder en mer modern och robust lösning för att hantera tid och datum.

Det finns också möjligheten att ställa in och ändra datumet med hjälp av funktioner som `mktime()` och `strftime()`. Det här är mer avancerade alternativ som kan vara användbara i vissa fall, men kräver lite mer kunskap om tid och datum hantering.

## Se även

1. [cppreference.com - time()](https://en.cppreference.com/w/cpp/chrono/c/time)
2. [cppreference.com - localtime()](https://en.cppreference.com/w/cpp/chrono/c/localtime)
3. [cppreference.com - tm struct](https://en.cppreference.com/w/cpp/chrono/c/tm)
4. [cppreference.com - <chrono> library](https://en.cppreference.com/w/cpp/chrono)