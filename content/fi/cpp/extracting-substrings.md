---
title:                "Alirivien erottaminen"
html_title:           "C++: Alirivien erottaminen"
simple_title:         "Alirivien erottaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus saattaa olla tarvetta hakea tai eritellä tiettyjä osia merkkijonosta, eli tällöin tarvitaan alimerkkijonojen etsimistä. Tämä toiminto on erittäin hyödyllinen esimerkiksi tekstin käsittelyssä tai tiedonhakupalveluissa.

## Kuinka tehdä

Voit helposti etsiä alimerkkijonoja C++-koodilla! Käytä funktiota `substr()` ja anna sille aloitusindeksi ja haluttu pituus alimerkkijonolle. Tässä on yksinkertainen esimerkki:

```C++
#include <iostream>
using namespace std;

int main() {
  string teksti = "Tässä on esimerkkiteksti.";
  string alimerkki = teksti.substr(11, 11);
  cout << alimerkki;

  return 0;
}

// Output:
// esimerkkiteksti
```

## Syvemmälle

`substr()`-funktio on käytössä vain C++:n kirjaston `string`-luokassa. Sen toiminta on myös hieman erilainen verrattuna esimerkiksi C-kielessä käytettyyn `substring()`-funktioon. `substr()` perii merkkijonon määrittelevät toiminnot, kuten pituuden ja indeksit. Lisäksi `substr()` tarjoaa myös mahdollisuuden asettaa oletusarvoisen pituuden, jos pituutta ei anneta parametrina.

## Katso myös

- [substr() dokumentaatio (cppreference.com)](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Merkkijonon alimerkkijonon etsiminen (w3schools.com)](https://www.w3schools.com/cpp/cpp_strings_substrings.asp)
- [C++ ohjelmointikielen virallinen sivusto](https://isocpp.org)