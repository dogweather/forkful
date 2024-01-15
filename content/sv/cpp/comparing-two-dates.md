---
title:                "Jämföra två datum"
html_title:           "C++: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Vissa programmerare kan behöva jämföra två datum för att kontrollera om de är lika eller om ett är senare än det andra. Det kan vara användbart för att filtrera data eller för att verifiera om en viss tid har passerat.

## Så här gör du

För att jämföra två datum i C++ kan du använda "tm" strukturer och funktioner från tid.h biblioteket. Här är ett exempel på hur man jämför två datum och får ut ett resultat:

```C++
#include <iostream>
#include <ctime>

int main() {

    // Skapa tm strukturer för de två datum vi vill jämföra
    struct tm datum1 = { 0, 0, 0, 4, 7, 120 }; // Datum 1: 4 juli 2020
    struct tm datum2 = { 0, 0, 0, 15, 10, 120 }; // Datum 2: 15 oktober 2020

    // Konvertera tm strukturerna till time_t värden
    time_t tid1 = mktime(&datum1);
    time_t tid2 = mktime(&datum2);

    // Jämför tiderna och få ut ett resultat
    if (tid1 == tid2) {
        std::cout << "Datumen är samma." << std::endl;
    }
    else if (tid1 > tid2) {
        std::cout << "Datum 1 är senare än datum 2." << std::endl;
    }
    else {
        std::cout << "Datum 2 är senare än datum 1." << std::endl;
    }

    return 0;
}
```

Output:

```
Datum 2 är senare än datum 1.
```

## Djupdykning

För att förstå hur jämförelsen av två datum fungerar i C++, behöver man ha en grundläggande förståelse för tm strukturer och time_t värden. En tm struktur innehåller medlemmar för år, månad, dag, timme, minut, sekund och veckodag. Time_t är en fördefinierad typ som representerar antalet sekunder från epoken (en viss referenstidpunkt, vanligtvis 1 januari 1970).

När man jämför två datum, konverteras de först till time_t värden som sedan jämförs med hjälp av logiska operatorer som ==, > och <. Det är också viktigt att tänka på att detta bara jämför datum och inte tid på dagen.

## Se även

* [Documentation on tm struct and mktime function in C++](https://en.cppreference.com/w/cpp/chrono/c/mktime)
* [Tutorial on comparing dates and times in C++](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)