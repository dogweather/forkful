---
title:    "C++: Beräkning av ett datum i framtiden eller förflutna"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara en användbar funktion i många olika sammanhang, såsom att planera resor, hålla koll på födelsedagar eller förfallodatum för fakturor.

## Så här gör du

För att kunna beräkna ett datum i framtiden eller förflutna i C++, behöver du först och främst ha en grundläggande förståelse för datum och tidshantering i språket. Det finns flera olika sätt att utföra denna beräkning och nedan följer två exempel som använder standardbiblioteket i C++ för att illustrera hur det kan göras:

```C++
// Exempel 1: Beräkna ett datum i framtiden
#include <iostream>
#include <chrono>

int main() {
    // Definiera ett aktuellt datum
    auto now = std::chrono::system_clock::now();
    // Lägg till 7 dagar till det aktuella datumet
    auto future = now + std::chrono::hours(7*24);
    // Konvertera till en sträng och skriv ut resultatet
    auto futureString = std::chrono::system_clock::to_time_t(future);
    std::cout << "Datum om 7 dagar: " << std::ctime(&futureString) << std::endl;

    return 0;
}

// Exempel 2: Beräkna ett datum i förflutna
#include <iostream>
#include <chrono>

int main() {
    // Definiera ett aktuellt datum
    auto now = std::chrono::system_clock::now();
    // Dra av 30 dagar från det aktuella datumet
    auto past = now - std::chrono::hours(30*24);
    // Konvertera till en sträng och skriv ut resultatet
    auto pastString = std::chrono::system_clock::to_time_t(past);
    std::cout << "Datum för 30 dagar sedan: " << std::ctime(&pastString) << std::endl;

    return 0;
}
```

Resultaten för dessa exempel kommer att vara i Unix-timestamp-format, men detta kan enkelt konverteras till ett mer läsbart format med hjälp av standardbiblioteket.

## På djupet

För den som är intresserad av att lära sig mer om datum och tidshantering i C++, finns det flera olika funktioner och metoder som kan vara användbara vid beräkning av datum i framtiden eller förflutna. Dessa inkluderar bland annat `std::chrono::hours`, `std::chrono::system_clock::now()` och `std::chrono::system_clock::to_time_t()`.

Det finns också flera olika bibliotek och ramverk tillgängliga för att underlätta hanteringen av datum och tid i C++, såsom Boost.Date_Time och Howard Hinnant's date library.

## Se även

- [Boost.Date_Time](https://www.boost.org/doc/libs/1_67_0/doc/html/date_time.html)
- [Howard Hinnant's date library](https://github.com/HowardHinnant/date)