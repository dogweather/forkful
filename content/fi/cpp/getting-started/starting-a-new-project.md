---
date: 2024-01-20 18:03:15.293824-07:00
description: "How to: (\"Kuinka se tehd\xE4\xE4n:\") Aloitetaan yksinkertaisella \"\
  Hello, World!\" esimerkill\xE4 C++:ssa. T\xE4m\xE4 asettaa perustan uudelle projektille."
lastmod: '2024-04-05T21:53:58.444138-06:00'
model: gpt-4-1106-preview
summary: "(\"Kuinka se tehd\xE4\xE4n:\") Aloitetaan yksinkertaisella \"Hello, World!\"\
  \ esimerkill\xE4 C++:ssa."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: ("Kuinka se tehdään:")
Aloitetaan yksinkertaisella "Hello, World!" esimerkillä C++:ssa. Tämä asettaa perustan uudelle projektille.

```C++
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}
```

Kun suoritat yllä olevan koodin, näet seuraavan tulosteen:
```
Hello, World!
```

## Deep Dive ("Sukellus syvään päätyyn")
Ennen C++11 standardia, C++ -koodit alkoivat yksinkertaisemmilla otsikkotiedostoilla ja usein käyttivät `printf` tyylisiä funktioita C:n perinnön vuoksi. C++11 toi mukanaan tyypin päättelyn (`auto`), alustajalistoja ja muita parannuksia, jotka tekevät projektin aloittamisesta nopeampaa ja mielekkäämpää.

Vaihtoehtoina perinteisen pääohjelman kirjoittamiselle, voit myös käyttää erilaisia C++ kirjastoja ja frameworkkeja, kuten Qt työpöytäsovellusten kehittämiseen tai Boost kirjastoa monimutkaisempiin algoritmeihin.

Projektin aloittamisen yksityiskohdissa on tärkeää miettiä, mitä rakennustyökaluja käytät, esimerkiksi CMake tai Makefile, ja miten projektin rakenne jäsennellään. Tämä sisältää kansiorakenteet, modulaarisuuden ja riippuvuuksien hallinnan.

## See Also ("Katso myös")
- [C++ Standards Committee Papers](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/)
- [cppreference.com](https://en.cppreference.com/w/)
- [The C++ Resources Network](https://isocpp.org/)
- [Boost C++ Libraries](https://www.boost.org/)
