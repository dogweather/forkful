---
title:    "C++: Beregning av dato i fremtiden eller fortiden."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

#Hvorfor

Det er mange grunner til at noen ville være interessert i å beregne en dato i fremtiden eller fortiden. Kanskje du trenger å planlegge en ferie, eller kanskje du ønsker å finne ut nøyaktig hvor mange dager du har vært i jobb. Uansett årsak, er det nyttig å kunne dataprogrammer for å gjøre disse beregningene for deg.

#Hvordan

En måte å beregne en dato i fremtiden eller fortiden er å bruke C++'s `chrono` bibliotek. Først må du inkludere biblioteket i koden din:

```C++
#include <iostream>
#include <chrono>
```

Deretter kan du bruke funksjonen `system_clock::now()` for å få gjeldende tidspunkt og beregne en dato i fremtiden eller fortiden. For å beregne en dato i fremtiden, må du legge til et visst antall dager til gjeldende tidspunkt, for eksempel:

```C++
auto now = std::chrono::system_clock::now();

// legge til 30 dager
auto future_date = now + std::chrono::hours(30 * 24);
```

For å beregne en dato i fortiden, må du trekke fra et visst antall dager fra gjeldende tidspunkt, for eksempel:

```C++
auto now = std::chrono::system_clock::now();

// trekke fra 30 dager
auto past_date = now - std::chrono::hours(30 * 24);
```

Du kan deretter bruke funksjonen `time_point_cast` for å konvertere datoen til et spesifikt format, for eksempel et `time_t` objekt som representerer antall sekunder siden starten av Unix-epoken:

```C++
auto now = std::chrono::system_clock::now();

// konvertere til `time_t` objekt
auto future_date = std::chrono::time_point_cast<std::chrono::seconds>(now);
auto past_date = std::chrono::time_point_cast<std::chrono::seconds>(now);

// få antall sekunder siden starten av Unix-epoken
std::time_t future_seconds = future_date.time_since_epoch().count();
std::time_t past_seconds = past_date.time_since_epoch().count();
```

#Dypdykk

Når du beregner en dato i fremtiden eller fortiden, er det viktig å være klar over at det ikke alltid er like enkelt som å legge til eller trekke fra et visst antall dager. Datoer kan være avhengig av ulike faktorer, som for eksempel skuddår, forskjellige antall dager i måneden og ulike tids soner.

Det er derfor viktig å være nøye med å validere og justere datoen din etter disse faktorene for å sikre nøyaktigheten av beregningen. Du kan også vurdere å bruke C++'s `chrono` bibliotek for å håndtere disse utregningene på en mer presis og robust måte.

#Se Også

- [C++ `chrono` biblioteket](https://www.cplusplus.com/reference/chrono/)
- [Beregning av datoer og tider i C++ med `chrono`](https://www.learncpp.com/cpp-tutorial/basier-chrontimepoints-and-durations/)