---
title:    "C++: Debug-tulostuksen tulostaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Usein C++ -ohjelmoijat käyttävät koodin suoritusvirheiden korjaamiseen tiedon tulostamista. Tämä tapahtuu tulostamalla debug-viestejä, jotka auttavat heitä ymmärtämään, mitä koodi tekee ja missä mahdolliset ongelmat voivat olla. Tämä on erittäin hyödyllinen työkalu, jolla voit nopeasti löytää ja korjata bugeja, joten sen käyttöä kannattaa harkita.

## Miten

"```C++
#include <iostream>

int main() {
  int x = 5;
  int y = 10;
  std::cout << "x:n arvo on " << x << " ja y:n arvo on " << y << std::endl;
  int summa = x + y;
  std::cout << "Summa on " << summa << std::endl;
  return 0;
}
```

Tässä on yksinkertainen esimerkki debug-viestien käytöstä. Koodissa on alustettu kaksi muuttujaa, x ja y, ja lopussa tulostetaan niiden arvot sekä niiden summa. Tulostettu tieto auttaa meitä tarkistamaan, että arvot ovat oikein ja laskutoimitus toimii odotetulla tavalla.

## Syvällinen selitys

Debug-viestien käyttö ei ole vain yksittäinen komento, vaan pikemminkin ohjelmointiin liittyvä tapa. Kun käytät debug-viestejä, sinun täytyy miettiä, mitkä tiedot ovat tärkeitä ja miten voit tulostaa ne mahdollisimman tehokkaasti. Tiedon tulostaminen voi myös hidastaa ohjelman suorituskykyä, joten sen käyttöä kannattaa harkita vain silloin, kun se on todella tarpeen.

## Katso myös

- ["Troubleshooting C++ Using Debugging Techniques"](https://www.geeksforgeeks.org/troubleshooting-c-debugging-techniques/)
- ["Debugging in C++"](https://www.tutorialspoint.com/cplusplus/cpp_debugging_techniques.htm)
- ["Mastering Debugging in C++"](https://hackr.io/blog/mastering-debugging-cpp)