---
date: 2024-01-20 17:52:14.552323-07:00
description: "How to: (Kuinka tehd\xE4:) Ennen `iostream`:in ja `std::cout`:in aikaa,\
  \ `printf()` oli debug-tulostuksen kuningas C:ss\xE4. Monet k\xE4ytt\xE4v\xE4t edelleen\
  \ `printf()`\u2026"
lastmod: '2024-04-05T22:51:11.017430-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Ennen `iostream`:in ja `std::cout`:in aikaa, `printf()`\
  \ oli debug-tulostuksen kuningas C:ss\xE4."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to: (Kuinka tehdä:)
```C++
#include <iostream>

int main() {
    // Alustetaan muuttuja
    int luku = 42;

    // Debug-tulostus
    std::cout << "Debug: luku on " << luku << std::endl;

    // Jokin logiikka
    luku *= 2;

    // Lisää debug-tulostusta
    std::cout << "Debug: luku kerrottuna kahdella on " << luku << std::endl;

    return 0;
}
```

Output:
```
Debug: luku on 42
Debug: luku kerrottuna kahdella on 84
```

## Deep Dive (Syväsukellus)
Ennen `iostream`:in ja `std::cout`:in aikaa, `printf()` oli debug-tulostuksen kuningas C:ssä. Monet käyttävät edelleen `printf()` myös C++-ohjelmoijat. Vaihtoehtoina on erilaiset kirjastot ja työkalut, kuten GDB tai Visual Studio Debugger, jotka tarjoavat monipuolisempia debuggausvaihtoehtoja kaivautua syvemmälle koodiin. C++20 toi meille `std::format`, joka tuo `printf()`:in mukavuudet moderniin C++:aan. Käyttämällä debug-tulostusta, voidaan tarkastella muuttujien arvoja ajon aikana ja ymmärtää paremmin ohjelman tila.

## See Also (Katso Myös)
- [cppreference.com, std::cout](https://en.cppreference.com/w/cpp/io/cout)
- [cppreference.com, std::format](https://en.cppreference.com/w/cpp/utility/format)
- [GNU Project, GDB: The GNU Project Debugger](https://www.gnu.org/software/gdb/)
- [Visual Studio Docs, Debugging in Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-in-visual-studio)
