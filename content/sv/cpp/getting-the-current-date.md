---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att hämta och använda det aktuella datumet i en C++-kod är något alla programmerare står inför. Vi behöver göra det för att hålla koll på händelser, logga data, skapa tidstemplar och för många andra viktiga uppgifter.

## Hur man gör:

Hämta det nuvarande datumet i C++ är enkelt med biblioteket `<chrono>`. Nedan är en kodsnutt:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto nu = std::chrono::system_clock::now();
    std::time_t nu_t = std::chrono::system_clock::to_time_t(nu);
    std::cout << "Det nuvarande datumet och tiden är: " << std::ctime(&nu_t) << std::endl;
    return 0;
}
```

Om du kör denna kod, kommer utdata ser ut något så här:

```
Det nuvarande datumet och tiden är: Thu Jan 20 14:20:50 2022
```

## Djup Dykning

För att förstå detta kodsnutt helt, finns det några punkter att notera. Dessa inkluderar historisk kontext, alternativ och implementation detaljer.

Det bibliotek vi använde här, `<chrono>`, är en del av C++11 och senare. Det förser oss med klasser och funktioner för att mäta tid. 

För att få det nuvarande datumet i C++, kan vi också använda `time_t`-typen från `<ctime>` bibliotek. Men, `<chrono>` är mer flexibel och precist. Båda dessa bibliotek förändrar inte systemklockan.

När du vill ha ännu mer exakt tidsmätning, skulle `<chrono>` vara ditt bästa val. Dess noggrannhet kommer från dess förmåga att mäta tidsintervaller i nanosekunder.

## Se även

Om du är intresserad av att lära dig mer om tidshantering i C++, kolla in dessa resurser:

- [cppreference.com: <chrono>](https://en.cppreference.com/w/cpp/chrono) 
- [cplusplus.com: Time library](http://www.cplusplus.com/reference/ctime/) 
- [StackOverflow: Getting current date and time](https://stackoverflow.com/questions/997946/how-to-get-current-time-and-date-in-c)