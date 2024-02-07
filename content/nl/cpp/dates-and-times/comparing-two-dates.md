---
title:                "Twee datums vergelijken"
date:                  2024-01-28T21:56:30.610924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het vergelijken van data gaat over het uitvogelen welke van twee data vroeger, later is, of dat ze hetzelfde zijn. Programmeurs doen dit voor het organiseren van evenementen, het laten verlopen van promoties, planning, herinneringen - in principe alles met een tijdcomponent.

## Hoe:
C++ maakt het leven makkelijk met de `<chrono>` header.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    using namespace std::chrono;

    // Maak system_clock tijdpunten
    system_clock::time_point vandaag = system_clock::now();
    system_clock::time_point eenDag = system_clock::now() - hours(24); // Gisteren

    // Omzetten naar time_t voor vergelijking
    time_t vandaag_time_t = system_clock::to_time_t(vandaag);
    time_t eenDag_time_t = system_clock::to_time_t(eenDag);

    if (vandaag_time_t > eenDag_time_t) {
        std::cout << "Vandaag is na eenDag.\n";
    } else if (vandaag_time_t < eenDag_time_t) {
        std::cout << "Vandaag is voor eenDag.\n";
    } else {
        std::cout << "Data zijn hetzelfde.\n";
    }

    return 0;
}
```

Voorbeelduitvoer:

```
Vandaag is na eenDag.
```

## Diepgaand:
Sinds C++11 is `<chrono>` de plaats voor datum en tijd. Daarvoor was je waarschijnlijk aan het worstelen met `<ctime>` en structuren zoals `tm`. Niet mooi.

Alternatieven? Zeker, er zijn bibliotheken van derden zoals Boost.DateTime. Maar waarom het ingewikkeld maken als `<chrono>` er direct is en evolueert.

Implementatiedetails om in je achterzak te houden:
- `std::chrono` gaat om tijdpunten en duur.
- `system_clock` meet de echte wereldtijd.
- `time_point` is een specifiek punt in tijd (bijv. een datum).
- `time_t` is een rekenkundig type, handig voor vergelijkingen.

## Zie ook:
- C++ Referentie voor `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Vergelijking van datum- en tijdbibliotheken: http://www.boost.org/doc/libs/1_64_0/doc/html/date_time.html
- Goede oude `<ctime>`, als je nostalgisch of masochistisch bent: https://en.cppreference.com/w/cpp/header/ctime
