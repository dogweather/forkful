---
title:                "C++: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme etsiä merkkijonon pituutta? Todennäköisesti jossain vaiheessa ohjelmointia joudumme käsittelemään merkkijonoja ja niiden ominaisuuksia. Merkkijonon pituuden löytäminen on tärkeä taito, jota tarvitaan monissa ohjelmointitehtävissä.

## Miten

Merkkijonon pituus voidaan helposti löytää käyttämällä `length()` -funktiota. Tämä funktio palauttaa merkkijonon pituuden kokonaislukuna.

```C++ 
#include <iostream>
#include <string>

using namespace std;

int main()
{
    string s = "Tämä on esimerkki";
    cout << "Merkkijonon pituus on: " << s.length() << endl;
}
```
**Output:**
Merkkijonon pituus on: 18

## Syvempi sukellus

Miksi käyttää `length()` -funktiota sen sijaan, että laskettaisiin merkkien määrä itse? Tähän on useita syitä. Ensinnäkin, käyttämällä valmiiksi määriteltyä funktiota säästämme aikaa ja vaivaa, sillä funktio on optimoitu ja tehokas. Toiseksi, se takaa oikean tuloksen, sillä mukana tulevat turvatoimet välttävät virheellisiä laskutoimituksia. Lisäksi, jos käytämme esimerkiksi `for` -silmukkaa merkkijonon läpikäymiseen, `length()` -funktion käyttö helpottaa koodin ymmärtämistä ja vähentää virheiden mahdollisuutta.

## Katso myös

- [Stringin käyttö C++:ssa](https://www.cplusplus.com/doc/tutorial/ntcs/)
- [Merkkijonojen käsittely C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Funktioiden käyttö C++:ssa](https://www.geeksforgeeks.org/functions-in-cpp/)