---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
date:                  2024-01-20T17:28:36.793597-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lasketaan tulevaisuuden tai menneisyyden päivämäärä. Tarvitaan esimerkiksi määrittämään eräpäiviä tai ajastamaan tapahtumia.

## How to: (Kuinka tehdä:)
```C++
#include <iostream>
#include <chrono>
#include <iomanip>
#include <ctime>

int main() {
    using namespace std::chrono;

    // Tämän päivän päivämäärä
    system_clock::time_point today = system_clock::now();
    
    // Muutetaan aika time_t:ksi tulostusta varten
    time_t t = system_clock::to_time_t(today);
    std::cout << "Tänään on: " << std::put_time(std::localtime(&t), "%F") << '\n';

    // Laske tulevaisuuden päivämäärä lisäämällä 30 päivää
    system_clock::time_point future_date = today + days{30};
    t = system_clock::to_time_t(future_date);
    std::cout << "30 päivän päästä: " << std::put_time(std::localtime(&t), "%F") << '\n';

    // Menneisyyden päivämäärä vähentämällä 30 päivää
    system_clock::time_point past_date = today - days{30};
    t = system_clock::to_time_t(past_date);
    std::cout << "30 päivää sitten: " << std::put_time(std::localtime(&t), "%F") << '\n';

    return 0;
}
```

Sample output:
```
Tänään on: 2023-04-07
30 päivän päästä: 2023-05-07
30 päivää sitten: 2023-03-08
```

## Deep Dive (Syvä sukellus):
Maailmassa joukoittain tapoja käsitellä aikaa, mutta C++20 toi standardikirjastoon `<chrono>` uudistuksia, helpottaen päivämäärien käsittelyä. Historiallisesti C++ käytti `<ctime>`, joka oli hankala. Esimerkiksi, aikavyöhykkeiden ja karkausvuosien kanssa oli ongelmia.

C++11 esitteli `<chrono>`, mutta vasta C++20 toi kunnolliset kalenteritoiminnot. Nyt voimme laskea päivämääriä helppokäyttöisesti ja tyypin turvallisesti.

Vaihtoehtoina ovat kolmannen osapuolen kirjastot kuten Boost.Date_Time. Mutta nyt, kun standardikirjasto tarjoaa hyvät välineet, ulkopuolisten kirjastojen tarve vähenee.

## See Also (Katso myös):
- C++20 `<chrono>` documentation: https://en.cppreference.com/w/cpp/chrono
- Historical reasons for `<ctime>`: https://en.wikipedia.org/wiki/C_standard_library#ctime
- Boost.Date_Time library: https://www.boost.org/doc/libs/release/libs/date_time/
