---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng innebär att förvandla ett datumformat till en läsbar textsträng. Programmerare gör detta för att underlätta datumhantering och förbättra anpassningsförmågan i presentationer av datuminformation.

## Hur man gör:

Här är en kodexempel i C++ för att konvertera ett datum till en sträng:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <sstream>

std::string datumTillStrang() {
    auto nu = std::chrono::system_clock::now();
    std::time_t tidsStämpel = std::chrono::system_clock::to_time_t(nu);
    std::tm* tidsDutt = std::localtime(&tidsStämpel);
    std::stringstream strang;
    strang << std::put_time(tidsDutt, "%Y-%m-%d");
    return strang.str();
}

int main() {
    std::cout << "Dagens datum är: " << datumTillStrang() << "\n";
    return 0;
}
```
Exempelutskrift kommer se ut såhär:

```C++
Dagens datum är: 2022-11-03
```

## Djupgående:

Denna konverteringsfunktionalitet har en lång historia, alltifrån tidiga programmeringspråk där datum representerades som grundläggande datatyp till moderna tillämpningar i kraftiga språk som C++.

Alternativet till detta koncept i C++ är att använda `boost::gregorian::to_iso_string`. Det är i princip samma sak, men mer sammanflätad i `boost`-funktioner.

Denna implementation använder `std::chrono` och `std::put_time`. Funktionen `std::put_time` konverterar `std::tm`-objektet till datumsträngen, using en formateringssträng som definierar formatet.

## Se även:

[`strftime`](http://www.cplusplus.com/reference/ctime/strftime/): En C-funktion för att formatera tiden som en sträng.\
[`std::put_time`](http://en.cppreference.com/w/cpp/io/manip/put_time): En C++-funktion för att formatera tiden som en sträng.\
[`boost::gregorian::to_iso_string`](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time/gregorian.html): En alternativ funktion för att konvertera datum till sträng i Boost-biblioteket.