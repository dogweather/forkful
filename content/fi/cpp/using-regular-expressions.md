---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "C++: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Säännölliset lausekkeet ovat tapa suodattaa, hakea ja muokata tekstejä C++ -ohjelmoinnissa. Voit käyttää niitä esimerkiksi validoidaksesi käyttäjän syötteen, etsiäksesi tiettyjä sanoja tai muuttaaksesi merkkijonojen muotoa. Ohjelmoijat käyttävät säännöllisiä lausekkeita tehdäkseen tekstin käsittelystä nopeampaa, helpompaa ja tarkempaa.

## Kuinka:

```C++
#include <iostream>
#include <regex>

int main() {
    std::string teksti = "Tämä on esimerkki tekstistä, jonka haluamme käsitellä.";
    
    // Muodostetaan säännöllinen lauseke haettavalle sanalle
    std::regex haku("esimerkki");
    
    // Käytetään regex_search -funktiota etsimään haettu sana tekstistä
    if(std::regex_search(teksti, haku)) {
        std::cout << "Haettu sana löytyi tekstistä!" << std::endl;
    } else {
        std::cout << "Haettua sanaa ei löytynyt tekstistä." << std::endl;
    }
    
    return 0;
}
```

Tuloste:

```
Haettu sana löytyi tekstistä!
```

## Syvä sukellus:

Säännöllisiä lausekkeita keksittiin jo 1950-luvulla ja ne ovat siitä lähtien olleet olennainen osa monien ohjelmointikielten toiminnallisuutta. C++:ssa käytetään POSIX-säännöllisiä lausekkeita, joita voi olla hieman haastava ymmärtää aluksi. Jos haluat helpomman tavan käyttää säännöllisiä lausekkeita, voit kokeilla bibliotekkeja kuten Boost tai std::experimental, jotka tarjoavat käyttöä helpottavia rajapintoja.

## Katso myös:

- [C++:n regex-dokumentaatio](https://en.cppreference.com/w/cpp/regex)
- [Boost-regex-dokumentaatio](https://www.boost.org/doc/libs/1_78_0/libs/regex/doc/html/boost_regex/ref/regex_token_iterator.html)
- [std::experimental -regex-dokumentaatio](https://en.cppreference.com/w/cpp/header/regex)