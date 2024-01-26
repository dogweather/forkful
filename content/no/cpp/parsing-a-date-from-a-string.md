---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:35:12.834107-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av datoer konverterer strenger til datotyper. Programmerere gjør dette for å håndtere datoer konsistent, utføre datooperasjoner og støtte internasjonalisering.

## Slik Gjør Du:
```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-03-28";
    std::istringstream ss(date_str);
    
    std::chrono::year_month_day parsed_date;
    ss >> std::chrono::parse("%F", parsed_date);
    
    if (ss.fail()) {
        std::cout << "Parsing failed.\n";
    } else {
        std::cout << "Parsed date is: " << parsed_date << '\n';
    }
    
    return 0;
}

/*
Output:
Parsed date is: 2023-03-28
*/
```

## Dypdykk
I gamle dager brukte C++ `std::tm` med `std::get_time` for parsing av datoer. Med C++20 introduseres `std::chrono` biblioteket som skaper en mer moderne og sikker måte å håndtere datoer og tider på. Alternativer inkluderer bruk av tredjeparts biblioteker som `boost::date_time`. Parsing involverer ofte feilsjekking, tidsonehåndtering og kulturavhengig format.

## Se Også
- [cppreference.com: std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Boost.Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
