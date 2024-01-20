---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:35:25.414984-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków polega na konwersji tekstowej reprezentacji daty do struktury data/czas, którą można potem łatwo manipulować w programie. Programiści robią to, aby umożliwić użytkownikom wprowadzanie dat w różnych formatach oraz przetwarzać i analizować informacje o czasie w aplikacjach.

## Jak to zrobić:
```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string date_str = "2023-04-02 14:20:12";
    std::tm tm = {};
    
    std::istringstream ss(date_str);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");  // format odpowiadający wzorcowi daty
    
    if (ss.fail()) {
        std::cerr << "Parsowanie daty nie powiodło się." << std::endl;
        return 1;
    }

    std::cout << "Dzień: " << tm.tm_mday << std::endl;
    std::cout << "Miesiąc: " << tm.tm_mon + 1 << std::endl;  // tm_mon jest od 0 do 11
    std::cout << "Rok: " << tm.tm_year + 1900 << std::endl;  // tm_year jest liczone od 1900
    std::cout << "Godzina: " << tm.tm_hour << std::endl;
    std::cout << "Minuta: " << tm.tm_min << std::endl;
    std::cout << "Sekunda: " << tm.tm_sec << std::endl; 
    return 0;
}
```
Sample Output:
```
Dzień: 2
Miesiąc: 4
Rok: 2023
Godzina: 14
Minuta: 20
Sekunda: 12
```

## Deep Dive
W przeszłości do parsowania dat używano funkcji takich jak `strptime` czy `strftime`. W nowoczesnym C++ warto używać biblioteki `<chrono>` oraz `<iomanip>` dla większej czytelności i bezpieczeństwa typów. Alternatywą dla wbudowanych funkcji jest także użycie bibliotek zewnętrznych, jak `boost::date_time`. W przypadku `std::get_time`, jest to funkcja manipulatora strumienia, która działa razem z `std::istringstream` w celu przekonwertowania stringa na strukturę `std::tm`. Ważne jest, aby znać format daty, który chcemy odczytać, co określamy za pomocą specyfikatorów formatu jak `%Y`, `%m`, `%d`, itd.

## See Also
- [cppreference.com - std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [cppreference.com - <chrono> Library](https://en.cppreference.com/w/cpp/header/chrono)
- [Boost Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)