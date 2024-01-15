---
title:                "Å få gjeldende dato"
html_title:           "C++: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger trenger man å vite dagens dato og klokkeslett i et program, for eksempel for å logge hendelser eller for å lage et dynamisk interface. Dette kan enkelt gjøres ved å få tak i dagens dato og klokkeslett gjennom å bruke C++.

## Hvordan

Det er flere måter å få tak i dagens dato og klokkeslett i C++. Den enkleste måten er å bruke funksjonen `std::chrono::system_clock::now()`. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```C++
#include <iostream>
#include <chrono> 

int main() {
    // Få tak i dagens dato og klokkeslett
    auto currentTime = std::chrono::system_clock::now();

    // Konverter til tidsformat
    std::time_t current_time = std::chrono::system_clock::to_time_t(currentTime);

    // Skriv ut resultatet
    std::cout << "Dagens dato og klokkeslett: " << std::ctime(&current_time) << std::endl;

    return 0;
}
```

Kjører man dette programmet vil man få en utskrift av dagens dato og klokkeslett på formatet "dag måned dato klokkeslett år". For eksempel "Tir 25 Juni 22:30:50 2019".

## Dypdykk

Hvis man ønsker å få tak i dagens dato på et mer spesifikt format, for eksempel bare dato uten klokkeslett eller bare måned og år, kan man bruke funksjonene `std::chrono::day` og `std::chrono::month` sammen med funksjonen `std::strftime()` for å formatere dato og klokkeslett. Her er et eksempel på hvordan man kan bruke disse funksjonene:

```C++
#include <iostream>
#include <chrono> 

int main() {
    // Få tak i dagens dato og klokkeslett
    auto currentTime = std::chrono::system_clock::now();

    // Hent ut dag, måned og år
    std::chrono::day day = std::chrono::day(currentTime);
    std::chrono::month month = std::chrono::month(currentTime);

    // Konverter til ønsket format
    char buffer[80];
    std::strftime(buffer, 80, "%d %B %Y", &tm);
    std::cout << "Dagens dato er: " << buffer << std::endl;

    return 0;
}
```

Dette eksempelet vil gi en utskrift av dagens dato på formatet "dag måned år", for eksempel "25 Juni 2019". Man kan også endre på formatet ved å endre på argumentene til `std::strftime()`.

## Se også

- [C++ Date and Time Library](https://en.cppreference.com/w/cpp/chrono)
- [How to Get Current Date and Time in C++](https://www.programiz.com/cpp-programming/library-function/ctime/time)