---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:14.552323-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Kun koodaat, joskus haluat kurkata ohjelman toimintaan – se on debug-tulostus. Se auttaa sinua näkemään, missä pisteessä ohjelma mättää tai miten data liikkuu.

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
