---
title:                "C++: Alastringien erottaminen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa ohjelmointia, koska se mahdollistaa merkkijonojen manipuloinnin ja käsittelyn. Tämä voi olla hyödyllistä esimerkiksi tietokantaohjelmoinnissa tai tekstin käsittelyssä.

## Kuinka tehdä

Substringien erottaminen C++:lla on helppoa. Ensimmäiseksi tarvitaan merkkijono, josta haluat erottaa alimerkkijonon. Tämän jälkeen käytetään `substr()` -funktiota, joka ottaa parametreina alkuindeksin ja loppuindeksin ja palauttaa erillisen alimerkkijonon.

```C++
#include <iostream>

using namespace std;

int main() {
  string s = "Hei maailma!";
  string sub = s.substr(4, 6); // aloittaa indeksistä 4 ja ottaa 6 merkkiä
  cout << sub << endl; // tulostaa "maailma"
  return 0;
}
```

Tässä esimerkissä otetaan merkkijonosta "Hei maailma!" alimerkkijono "maailma" aloittaen indeksistä 4 ja ottamalla 6 merkkiä.

## Syvemmälle

C++ tarjoaa myös muita tapoja erottaa alimerkkijonoja. Esimerkiksi `find()` -funktio löytää annetun merkkijonon ensimmäisen esiintymän ja palauttaa sen indeksin. Tämän tiedon avulla voidaan käyttää `substr()` -funktiota erottamaan haluttu alimerkkijono.

```C++
#include <iostream>

using namespace std;

int main() {
  string s = "Tämä on esimerkkiteksti";
  int index = s.find("esimerkki"); // löytää merkkijonon "esimerkki" indeksin
  string sub = s.substr(index, 9); // aloittaa löydetyltä indeksiltä ja ottaa 9 merkkiä
  cout << sub << endl; // tulostaa "esimerkki"
  return 0;
}
```

Substringien erottaminen voi myös olla hyödyllistä tiedon tallentamisessa ja käsittelyssä, jos halutaan esimerkiksi tallentaa tietyn tiedoston tiettyjen osien sisältö erillisiin muuttujiin.

## Katso myös

- [C++ string class](https://www.cplusplus.com/reference/string/string/) (C++-merkkijonojen luokka)
- [Substring manipulation in C++](https://www.geeksforgeeks.org/substring-manipulation-in-c-string-find-substr/) (Substringien käsittely C++:ssa)
- [String functions in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm) (Merkkijonofunktiot C++:ssa)