---
title:                "Alimerkkijonojen erottaminen"
html_title:           "C++: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringien erottaminen, eli niin kutsuttu substringien erottaminen, on tapa, jolla voidaan poimia osia merkkijonoista tiettyjen kriteerien perusteella. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan käsitellä vain tietyn osan sisältävä merkkijono. Ohjelmoijat käyttävät tätä tekniikkaa parantaakseen koodin suorituskykyä ja tarkkuutta.

## Kuinka:
```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // Alustetaan esimerkkimerkkijono
  string s = "Tervetuloa Suomeen";

  // Poimitaan merkkejä 7.-14. indeksin väliltä, eli "Suomeen"
  string substring = s.substr(7, 7);

  // Tulostetaan poimittu osa
  cout << substring << endl;

  return 0;
}
```

Tulos:
```
Suomeen
```

## Syventymistä:
Substringien erottaminen on ollut osa ohjelmoinnin maailmaa jo pitkään, mutta modernit kielet, kuten C++, tarjoavat helppokäyttöisiä työkaluja tähän tarkoitukseen. Toisinaan voit törmätä myös termiin "slice", joka viittaa samaan asiaan. Toisin kuin monissa muissa kielissä, C++:ssa substringien indeksointi lähtee nollasta, joten ensimmäinen merkki on indeksissä 0 eikä 1.

## Katso myös:
- [C++:n virallinen dokumentaatio substringien erottamisesta](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Tietoja substringeista ja slicing-tekniikasta](https://www.geeksforgeeks.org/slicing-string-python/)
- [String-funktioita ja niiden hyödyntämistä C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)