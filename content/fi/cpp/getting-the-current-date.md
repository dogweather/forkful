---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:13:09.749915-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"
Haet nykyistä päivämäärää, koska sovellukset tarvitsevat kontekstia. Syntymäpäivien seuranta, aikaleimat tai päivittäiset tehtävät – päivämäärät ovat tärkeitä.

## How to:
"Kuinka:"
C++17 sisältää kirjaston `<chrono>`, joka helpottaa nykyisen päivämäärän noutamista. Tässä käytännön esimerkki:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

int main() {
    auto nyt = std::chrono::system_clock::now(); // Haetaan nykyhetki
    std::time_t aika_nyt = std::chrono::system_clock::to_time_t(nyt);
    
    // Tulostetaan hetki, standardeissa muodoissa
    std::cout << "Tänään on: " << std::put_time(std::localtime(&aika_nyt), "%Y-%m-%d") << std::endl;

    return 0;
}
```

Esimerkkituloste:

```
Tänään on: 2023-05-20
```

## Deep Dive
"Sukellus syvemmälle"
Ennen C++11-versiota käytettiin `ctime`-kirjastoa, mutta `<chrono>` teki kaikesta siistimpää ja turvallisempaa. Vaihtoehtoisia menetelmiä ovat esimerkiksi kolmansien osapuolien kirjastot tai alustakohtaiset kutsut. Implementointi `<chrono>`:ssa on tarkoituksellisesti abstraktoitu, jotta se toimii kaikkialla samalla tavalla.

## See Also
"Katso myös"
- C++-standardikirjasto: [cppreference.com](https://en.cppreference.com/w/)
- `<chrono>`-kirjaston syventävä opas: [C++ `<chrono>`](https://en.cppreference.com/w/cpp/chrono)
- C++:n aikaisemmat päivämäärä- ja aikakirjastot `ctime`: [C `<ctime>`](https://en.cppreference.com/w/c/chrono)
- ISO C++ -standardeista ja niiden kehityksestä: [isocpp.org](https://isocpp.org/)
