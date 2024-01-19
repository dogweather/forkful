---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att konvertera en sträng som representerar ett datum till ett effektivt minnesformat. Detta gör programmerare för att manipulera data, göra beräkningar och visa resultat på ett mer lämpligt eller förståeligt format för användaren.

## Hur man gör:

Låt oss dyka rakt in i kodexemplen i C++. Vi använder `std::get_time`, ett praktiskt biblioteksfunktion för att tolka datum och tid.
```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::tm tm = {};
    std::istringstream ss("2021-08-14");
    ss >> std::get_time(&tm, "%Y-%m-%d");
    
    if (ss.fail()) {
        std::cout << "Det gick inte att tolka datumsträngen.\n";
    } else {
        std::time_t time = mktime(&tm);
        if (time != -1) {
            std::cout << "Tolkat datum: " << std::asctime(std::localtime(&time));
        }
    }

    return 0;
}
```
När du kör koden får du:
```C++
'Tolkat datum: Sat Aug 14 00:00:00 2021'
```
## Djupdykning

Historiskt sett, före C++20, användes ofta manuella strängmanipuleringstekniker för att tolka datum. Det öppnade dock för fel och var i allmänhet inte så effektivt.

Alternativ till `get_time` inkluderar strängparsningsbibliotek som `boost::date_time` och `date.h` biblioteket. Nackdelen med dessa lösningar är att de kan vara tunga för att lösa specifika problem.

På implementeringsnivå konverterar `std::get_time` strängen till ett `std::tm` objekt, som internt representerar datum som separata fält (t.ex., år, månad, dag, timme, minut, sekund).

## Se även

För vidare läsning rekommenderas:
- [C++ Referens - get_time](http://www.cplusplus.com/reference/iomanip/get_time/)
- [Alternativ till date.h](https://stackoverflow.com/questions/11213326/how-to-parse-a-date-string-into-a-boostgregorian-date)
- [Formatering och tolkning av tid och datum](https://en.cppreference.com/w/cpp/io/manip/get_time)