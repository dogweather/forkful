---
title:                "C++: Merkkijonon pituuden löytäminen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi löytää merkkijonon pituuden? On monia tilanteita, joissa tarvitsemme tätä tietoa esimerkiksi kun käsittelemme käyttäjän antamia syötteitä tai kun haluamme tarkistaa, että merkkijono mahtuu määriteltyyn muuttujan kokoon.

## Miten

Merkkijonojen pituuden löytäminen on varsin helppoa C++:ssa. Käytämme siihen standardikirjaston funktiota, joka on nimeltään `size()`. Alla on esimerkki, miten sitä käytetään:

```
#include <iostream>
#include <string>

int main() {
  // Määritellään merkkijono
  std::string s = "Tämä on esimerkkilause.";

  // Tulostetaan merkkijonon pituus
  std::cout << "Merkkijonon pituus on " << s.size() << " merkkiä." << std::endl;

  return 0;
}
```

Tämä koodi tulostaa seuraavan tekstin: "Merkkijonon pituus on 22 merkkiä."

## Syväsukellus

Kun katsomme tarkemmin `size()`-funktiota, huomaamme, että se palauttaa `size_t` tietotyypin arvon. Tämä tarkoittaa, että se palauttaa merkkijonon pituuden kokonaislukuna. On myös hyvä muistaa, että merkkijonon pituus sisältää myös välilyöntimerkit ja muut erikoismerkit.

Jos haluamme käyttää merkkijonon pituutta ehtolauseissa tai muissa laskutoimituksissa, voimme tallentaa sen muuttujaan ja käyttää sitä tarpeen mukaan.

```
#include <iostream>
#include <string>

int main() {
  // Määritellään merkkijono
  std::string s = "Tämä on esimerkkilause.";

  // Tallennetaan merkkijonon pituus muuttujaan
  size_t pituus = s.size();

  // Käytetään muuttujaa ehtolauseessa
  if (pituus < 50) {
    std::cout << "Merkkijono on sopivan lyhyt." << std::endl;
  }
  
  // Käytetään muuttujaa laskussa
  std::cout << "Merkkijonon pituus on " << pituus * 2 << " merkkiä, kun se tuplataan." << std::endl;

  return 0;
}
```

Tämä koodi tulostaa seuraavan tekstin: "Merkkijono on sopivan lyhyt." sekä "Merkkijonon pituus on 44 merkkiä, kun se tuplataan."

## Katso myös

- [C++ Ohjelmointiopas](https://www.tutorialspoint.com/cplusplus/index.htm)
- [C++ Merkkijonot](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
- [C++ Standardikirjasto](https://en.cppreference.com/w/cpp/header)