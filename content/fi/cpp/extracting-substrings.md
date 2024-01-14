---
title:    "C++: Alaohjelmien poimiminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on hyödyllinen taito, jota tarvitaan usein ohjelmoinnissa. Se mahdollistaa merkkijonojen käsittelyn ja manipuloinnin, mikä on tärkeää monissa ohjelmointitehtävissä. Seuraavassa oppaassa opit kuinka on mahdollista erottaa alimerkkijonoja C++:ssa.

## Kuinka tehdä niin

Substringsin erottaminen C++:ssa on helppoa. Alla on esimerkki, kuinka erotat alimerkkijonon annetusta merkkijonosta käyttäen `substr()` -funktiota:

```C++
#include <iostream>
using namespace std;

int main() {
    string s = "Tämä on esimerkkimerkkijono.";
    string sub = s.substr(0, 4); //erotetaan ensimmäiset 4 merkkiä
    cout << sub << endl; //tulostaa "Tämä"

    return 0;
}
```

Tässä koodissa `substr()`-funktio ottaa kaksi parametria: ensimmäisen ja viimeisen indeksin, joiden väliseen alueeseen alimerkkijono halutaan erottaa. On tärkeää huomata, että `substr()` palauttaa aina uuden merkkijonon ja ei muuta alkuperäistä merkkijonoa.

## Syvemmälle

On hyödyllistä tietää, miten `substr()`-funktio toimii taustalla. Se ottaa parametrit `unsigned int` tai `size_t` muodossa, joten tarkkaavaisuus on tarpeen, koska negatiivisten kelvottomat arvot johtavat epäloogiseen toimintaan. Lisäksi `substr()` käyttää iteraattoreita erottaakseen alimerkkijonon. Esimerkiksi `substr(3, 5)` ottaa muuttujan alkuperäisestä merkkijonosta, joka alkaa 3. indeksistä ja päättyy 7. indeksiin (mutta ei sisällä sitä).

## Katso myös

- [std::string::substr - C++ Reference](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [C++ Strings - GeeksforGeeks](https://www.geeksforgeeks.org/cpp-strings/)
- [C++ Standard Library - cppreference.com](https://en.cppreference.com/w/)