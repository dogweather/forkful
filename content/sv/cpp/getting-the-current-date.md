---
title:                "C++: Att få aktuellt datum"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att prata om hur man hämtar aktuellt datum i C ++. Att veta det aktuella datumet kan vara användbart för många program, till exempel för att visa datum på en webbsida eller för att hålla koll på uppdateringar i ett program.

## Så här gör du

Hämta aktuellt datum i C++ är enkelt med hjälp av standardbiblioteket <ctime>. Först måste vi inkludera biblioteket i vårt program. Sedan kan vi använda funktionen time() för att hämta antalet sekunder som har gått sedan 1 januari 1970.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Hämta nuvarande tid i sekunder
    time_t now = time(0);

    // Konvertera tiden till datum och spara det i en sträng
    std::string date = ctime(&now);

    // Skriv ut det aktuella datumet
    std::cout << "Aktuellt datum: " << date << std::endl;

    return 0;
}
```

Kör programmet och du kommer att få utskriften "Aktuellt datum: <datum och tid>". Funktionen ctime() konverterar antalet sekunder till ett datum i formatet "Www Mmm dd hh:mm:ss yyyy", där Www är veckodagen, Mmm är månaden, dd är datumet, hh:mm:ss är tiden och yyyy är året.

## Djupdykning

För att förstå mer om hur man hämtar aktuellt datum i C++, är det viktigt att veta att funktionen time() returnerar ett objekt av typen time_t. Detta objekt innehåller antalet sekunder som har gått sedan 1 januari 1970.

Ett vanligt sätt att konvertera detta värde till ett datum är att använda funktionen ctime(), som vi gjorde i vårt exempel ovan. Men det finns också andra funktioner som kan användas för att konvertera tiden till ett datum, såsom gmtime(), localtime() och strftime().

Det finns också möjligheter att ändra formatet på datumet som returneras av ctime(). Till exempel kan du använda funktionen strftime() och ange ett eget format för datumet, som "dd/MM/yyyy" för att få utskriften i formatet "dd/MM/yyyy" istället för "Www Mmm dd hh:mm:ss yyyy".

## Se även

- Lär dig mer om <ctime> biblioteket: https://www.cplusplus.com/reference/ctime/
- Utforska olika sätt att manipulera datum i C++: https://www.codeproject.com/Tips/154264/Various-ways-of-generating-a-date-in-a-centralized