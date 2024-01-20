---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Nykyisen päivämäärän hakeminen C++: lla

## Mikä & Miksi?

Nykyisen päivämäärän hankkiminen C++:lla on tapa saada tietokoneeltasi tämänhetkinen vuosi, kuukausi ja päivä. Ohjelmoijat tarvitsevat tätä tietoa moniin asioihin, kuten lokitiedostojen luomiseen tai ajan leimojen asettamiseen tietoihin.

## Näin teet:

Käytämme C++17:aa ja `chrono` -kirjastoa tässä esimerkissä.

```C++
#include <chrono>
#include <iostream>
#include <iomanip>

int main()
{
    auto nykyaika = std::chrono::system_clock::now();
    std::time_t aika = std::chrono::system_clock::to_time_t(nykyaika);

    std::cout << "Nykyinen päivä: " << std::put_time(std::localtime(&aika), "%Y-%m-%d") << '\n';

    return 0;
}
```

Mahdollinen tuloste:

```
Nykyinen päivä: 2022-09-21
```
## Syvemmälle:

Viime vuosina C++:n standardi on kehittynyt tarjoamaan meille paremman tavan työskennellä päivämäärien ja ajan kanssa: `chrono`-kirjasto, joka lisättiin C++11:ssä ja sitä on paranneltu C++14:ssä ja C++17:ssä. Ennen `chrono`:a, ohjelmoijat luottivat C-kielen aikafunktioihin, kuten `time_t` ja `tm`.

C++20 tuo mukanaan kalenteripohjaisen päivämäärän tuen `chrono`-kirjastoon, mikä tekee päivämäärän käsittelystä vielä helpompaa.

## Katso myös:

Täydellinen opas `chrono`-kirjastoon: https://en.cppreference.com/w/cpp/chrono

C++17:n standardikirjaston dokumentointi: http://www.cplusplus.com/reference/chrono/

C++20:n uudet ominaisuudet, mukaan lukien kalenteriin perustuvat päivämäärät: https://en.cppreference.com/w/cpp/chrono/calendar