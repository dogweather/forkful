---
title:                "C++: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av to datoer er en vanlig oppgave i programmering, spesielt når det kommer til å håndtere datoer og tidsstempel. Det kan være nyttig å vite hvordan man sammenligner to datoer for å sortere data eller for å kontrollere om en dato ligger før eller etter en annen.

## Hvordan

I C ++ kan du sammenligne to datoer ved hjelp av Operator Overloading, som tillater at operatøren ">" og "<" brukes for å sammenligne objekter. La oss se på et eksempel på hvordan du kan sammenligne to datoer i kode:

```C++
// Inkluderer nødvendige biblioteker
#include <iostream>
#include <string>
#include <chrono>

// Definerer en funksjon for å sammenligne to datoer
bool compareDates(std::chrono::system_clock::time_point date1, std::chrono::system_clock::time_point date2) {
    return (date1 < date2);
}

// Hovedfunksjonen
int main()
{
    // Oppretter to datoer ved hjelp av chrono biblioteket
    std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point date2 = std::chrono::system_clock::now() + std::chrono::hours(24);

    // Kaller på funksjonen for å sammenligne datoene og skriver ut resultatet
    if (compareDates(date1, date2)) {
        std::cout << "Date 1 is earlier than Date 2";
    } else {
        std::cout << "Date 2 is earlier than Date 1";
    }

    return 0;
}
```

Output:
```
Date 1 is earlier than Date 2
```

Som du kan se, brukes < og >-operatørene for å sammenligne datoene, og resultatet avhenger av hvilken dato som ligger først. Du kan også bruke andre operatører som >= og <= for å sammenligne datoer.

## Dypdykk

Nå som du vet hvordan du sammenligner to datoer, kan det være nyttig å forstå hvordan datoenes format påvirker sammenligningsprosessen. I C ++ kan datoer representeres på flere måter, for eksempel ved hjelp av time_t- eller tm-strukturer. Det er viktig å være klar over hvilket format du arbeider med når du sammenligner datoer, da dette kan påvirke resultatet.

Et annet viktig poeng å merke seg er at tidssone kan påvirke sammenligningen av datoer. Når du sammenligner datoer, sammenligner du faktisk tidspunktet for de to datoene, og hvis de er i forskjellige tidssoner, kan dette føre til uventede resultater. Derfor er det viktig å være bevisst på tidssone når du arbeider med datoer og tidsstempel.

## Se også

- [C ++ time_t](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [C ++ chrono bibliotek](https://en.cppreference.com/w/cpp/chrono)