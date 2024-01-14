---
title:    "C++: Merkkijonon iso kirjoitus"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi: Miksi käyttäisi aikaa koodata merkkijonon isolla alkukirjaimella?

Kirjoittaessa koodia, saatat joutua kohtaamaan tilanteen, jossa haluat muuttaa merkkijonon alkukirjaimen isoksi. Tämä voi olla tarpeellista esimerkiksi silloin, kun haluat tulostaa tekstin käyttäjälle tai tallentaa tietokantaan. On olemassa valmiita funktioita, jotka hoitavat tämän puolestasi, mutta on myös hyvä tietää, miten se tehdään ohjelmallisesti.

## Kuinka: Koodiesimerkkejä ja tulosteita, selitettyinä "```C++ ... ```" koodilohkoissa.

Yksi tapa muuttaa merkkijonon ensimmäinen kirjain isoksi on käyttää funktiota `toupper()`. Tässä esimerkissä käytämme `toupper()`-funktiota muuttaaksemme merkkijonon "hello" ensimmäisen kirjaimen isommaksi ja tulostamme sen konsoliin.

```C++
#include <iostream>
#include <cstring>
#include <cctype>

int main()
{
    char str[] = "hello";
    str[0] = toupper(str[0]);

    std::cout << str;

    return 0;
}
```
Tuloste: "Hello"

Toinen tapa on käyttää `toupper()`-funktion sijasta `std::toupper()`-funktiota, joka käsittelee C++:n std::string-tyyppisiä merkkijonoja. Tässä esimerkissä käytämme `std::toupper()`-funktiota muuttaaksemme merkkijonon "world" ensimmäisen kirjaimen isommaksi ja tulostamme sen konsoliin.

```C++
#include <iostream>
#include <string>
#include <cctype>

int main()
{
    std::string str = "world";
    str[0] = std::toupper(str[0]);

    std::cout << str;

    return 0;
}
```
Tuloste: "World"

## Syvemmälle: Lisätietoja merkkijonon isoksi muuttamisesta.

Merkkijonon muuttaminen isoksi on yksi tärkeä osa ohjelmointia. On kuitenkin tärkeää ymmärtää, että se on vain yksi tapa manipuloida merkkijonoja. On olemassa muita funktioita ja metodeja, jotka voivat auttaa sinua muokkaamaan merkkijonoja haluamallasi tavalla. Kannattaa kokeilla erilaisia tapoja ja löytää itselleen sopivin ratkaisu.

## Katso myös:

- [toupper() C++ -referenssi (C++ Reference)](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [toupper() C -referenssi (C Reference)](https://en.cppreference.com/w/c/string/byte/toupper)
- [std::toupper() C++ -referenssi (C++ Reference)](https://en.cppreference.com/w/cpp/string/char_traits/toupper)
- [Ääkköset ja merkkijonon manipulointi (C++ Gazette Tutorials)](https://www.cplusplus.com/articles/541gkGSv/)