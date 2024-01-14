---
title:                "C++: Kalkulerer en dato i fremtiden eller fortiden"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å beregne en dato i fremtiden eller fortiden, for eksempel for å planlegge en jubileumsfeiring, beregne betalingsdatoer eller lage en påminnelse om en viktig hendelse. Ved å bruke C++, kan du enkelt lage et program som kan gjøre disse beregningene for deg.

## Hvordan

For å beregne en dato i fremtiden eller fortiden i C++, trenger du å bruke klassen "std::chrono::time_point" og funksjonen "std::chrono::duration" fra standardbiblioteket. Dette lar deg arbeide med tidspunkter og tidsintervaller i koden din. Se et eksempel på en funksjon som beregner datoen 30 dager frem i tid:

```
#include <iostream>
#include <chrono>

using namespace std;

int main() {
    // Lag et time_point som viser nåværende tidspunkt
    chrono::time_point<chrono::system_clock> now = chrono::system_clock::now();
    
    // Lag et duration-objekt som representerer 30 dager
    chrono::duration<int, std::ratio<86400>> days(30);
    
    // Beregn fremtidig dato ved å legge til duration
    chrono::time_point<chrono::system_clock> future = now + days;
    
    // Konverter til string og skriv ut
    string future_date = chrono::system_clock::to_string(future);
    cout << "Datoen 30 dager frem i tid er " << future_date << endl;
    
    return 0;
}
```

Eksempel på output:

```
Datoen 30 dager frem i tid er Sun Mar 22 03:51:32 2020
```

## Dykk dypere

I eksempelet over brukte vi "std::ratio<86400>" for å representere 30 dager. Denne ratioen representerer en dag i sekunder, og ved å endre verdien kan vi beregne datoer i ulike tidsintervaller, for eksempel timer, uker eller måneder. Det er også verdt å merke seg at "chrono::time_point" og "chrono::duration" er type-sikker og gjør det enkelt å unngå feil i beregningene dine.

## Se også

- Les mer om "std::chrono" biblioteket i C++ her: https://en.cppreference.com/w/cpp/chrono
- Se en ekstra kodeeksempel på hvordan beregne en dato i fremtiden eller fortiden her: https://www.geeksforgeeks.org/calculating-future-past-dates-based-given-date-c/