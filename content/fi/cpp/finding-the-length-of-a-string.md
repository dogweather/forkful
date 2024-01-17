---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "C++: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
"Merkkijonon pituuden löytäminen" tarkoittaa sitä, että ohjelmoija selvittää kuinka monta merkkiä merkkijono sisältää. Tämä on tärkeää, sillä ohjelmien on usein käsiteltävä merkkijonoja ja niiden pituutta tarvitsee tietää esimerkiksi silloin, kun halutaan varmistaa, että annettu merkkijono mahtuu tiettyyn muuttujaan.

## Miten:
Tehtävän suorittamiseksi käytetään C++:n `size()` -funktiota, joka palauttaa merkkijonon pituuden. Esimerkiksi:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string s = "Tämä on esimerkki merkkijonosta.";
    cout << s.size() << endl;
}
```

Tulostus:

`30`

## Syvemmälle:
Tämä merkkijonon pituuden laskemisen tapa on yleisesti käytössä modernissa ohjelmoinnissa. Kuitenkin ennen C++:aa merkkijonon pituuden selvittämiseen käytettiin `strlen()` -funktiota, joka löytyy edelleen C-kielestä ja joistain muista kielistä.

On myös mahdollista laskea merkkijonon pituus manuaalisesti käymällä läpi jokainen merkki ja lisäämällä muuttujaan yksi jokaisesta löydetystä merkistä. Tämä kuitenkin vie enemmän aikaa ja on alttiimpi virheille, joten `size()` -funktio on yleisesti suositeltava tapa hoitaa tehtävä.

## Katso myös:
- [C++ string size()](http://www.cplusplus.com/reference/string/string/size/)
- [C++ string length](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_length.htm)
- [C string strlen()](https://www.programiz.com/cpp-programming/library-function/cstring/strlen)