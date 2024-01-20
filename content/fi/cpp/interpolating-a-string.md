---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Merkkijonon interpolointi C++:ssa: Mitä, Miksi ja Miten?

## Mitä & Miksi?

Merkkijonon interpolointi on prosessi, jossa muuttujien tai lausekkeiden arvot sijoitetaan suoraan merkkijonoon. Se helpottaa koodin kirjoittamista, parantaa sen luettavuutta ja ylläpidettävyyttä.

## Miten se tehdään:

Katsotaan miten merkkijonon interpolointi tehdään C++:ssa (nykyversiossa, C++20).

```C++
#include <iostream>
#include <format>

int main() {
    std::string name = "Pekka";
    int age = 33;

    std::string message = std::format("Hei, nimeni on {} ja olen {} vuotta vanha", name, age);

    std::cout << message << std::endl;

    return 0;
}
```

Tulostus:

```bash
Hei, nimeni on Pekka ja olen 33 vuotta vanha
```

## Syvempi sukellus:

Merkkijonon interpolointi tulee skriptikielistä, kuten Perl ja Ruby. C++ sai tämän toiminnon vasta C++20-versiossa `std::format`-funktion myötä.

Vaihtoehtoisesti voit käyttää `sprintf`-funktiota tai `std::stringstream`-luokkaa, mutta ne ovat monimutkaisempia ja herkempiä virheille.

Merkkijonon interpoloitumisen yksityiskohdat: `std::format` ottaa muodon merkkijonon ja korvaa jokaisen "{}" paikanmuuttujan argumenttien arvoilla. Argumentit muunnetaan merkkijonoiksi ja lopputulos on interpoloitu merkkijono.

## Katso myös:

1. [C++ Reference-std::format](https://en.cppreference.com/w/cpp/utility/format)
2. [sprintf-Function](https://www.cplusplus.com/reference/cstdio/sprintf/)
3. [std::stringstream Class](http://www.cplusplus.com/reference/sstream/stringstream/)