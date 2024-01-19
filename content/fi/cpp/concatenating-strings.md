---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Merkkijonojen yhdistäminen, eli konkatenointi, on prosessi jossa kaksi tai useampi merkkijonoja yhdistetään yhdeksi. Tämä on todella hyödyllistä, kun tarvitset yhdistellä useita tietolähteitä yhdeksi lauseeksi tai teksteihin.

## Kuinka Tehdään:

```C++
#include <iostream>
#include <string>

int main() {
    std::string tervehdys = "Hei ";
    std::string nimi = "Matti!";
    std::string kokonainen_tervehdys = tervehdys + nimi;
    std::cout << kokonainen_tervehdys << std::endl;
    return 0;
 }
```
Tulostaa: ```Hei Matti!```

Käyttämällä `+` operaattoria, voimme yhdistää `tervehdys` ja `nimi` merkkijonot.

## Sukellus Syvemmälle:

Historiallisessa kontekstissa, C:ssa ei ollut sisäänrakennettua tukea merkkijonon konkatenoinnille. Tämä ongelma ratkaistiin C++:ssa sisällyttämällä merkkijonojen käsittelyyn tarkoitettu luokka ja `+` -operaattori.

On olemassa vaihtoehtoisia tapoja yhdistää merkkijonoja C++:ssa. Esimerkiksi `append()` -funktio, joka lisää merkkijonon toisen loppuun:

```C++
std::string str1 = "Hei ";
std::string str2 = "Matti!";
str1.append(str2);
std::cout << str1 << std::endl; // Tulostaa: "Hei Matti!"
```
Sisäpuolella, merkkijonojen yhdistäminen C++:ssa tapahtuu luomalla uusi merkkijono, joka sisältää yhdistettyjen merkkijonojen merkit. On tärkeää huomata, että suuret merkkijonot tai usein tapahtuva konkatenointi voivat olla suorituskyvyn kannalta haastavia, koska jokainen konkatenointi luo uuden merkkijonon.

## Katso Myös:

1. cppreference.com - C++ merkkijono luokka: [linkki](https://en.cppreference.com/w/cpp/string/basic_string)
2. cplusplus.com - Merkkijonojen yhdistäminen: [linkki](http://www.cplusplus.com/reference/string/string/append/)
3. Stack Overflow: vinkkejä ja temppuja merkkijonojen yhdistämiseen C++:ssa: [linkki](https://stackoverflow.com/questions/18892281/most-idiomatic-way-to-concatenate-strings)
4. C++ Standardeja ja laatua merkkijonojen hallintaan: [linkki](https://isocpp.org/wiki/faq/strings)