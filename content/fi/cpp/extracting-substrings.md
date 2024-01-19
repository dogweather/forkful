---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen alijonon poiminta on prosessi, jossa valitaan tietty osa merkkijonosta. Ohjelmoijat tekevät sen tarvittaessa tietoja tai redukoimaan datan määrää.

## Miten tehdään:

Hyödynnämme C++-ohjelmoinnissa `substr`-funktiota. Koodi näyttää tältä:

```C++
#include <iostream>
#include <string>
  
int main() {
    std::string str = "ohjelmointi";
    std::string alijono = str.substr(0, 4);
    std::cout << alijono;
    return 0;
}
```
Tämä koodi tuottaa seuraavan tulosteen:
```
ohje
```

## Syvempi sukellus:

`substr`-funktion historia ulottuu C++98-versioon asti, ja sitä on hyödynnetty laajasti sen joustavuuden ja tehokkuuden vuoksi. 

Vaihtoehtoinen tapa tehdä sama asia on käyttää `find`- ja `erase`-funktioita. Tämä menetelmä on kuitenkin monimutkaisempi ja hitaampi.

Substring-funktion suorituskyky ja tehokkuus riippuvat sen implementoinnista. Erityisesti, STL:n (Standard Template Library) `substr`-funktio käyttää laiskaa leikkausstrategiaa, joka parantaa merkittävästi suorituskykyä.

## Katso myös:

Lisätietoa löytyy näistä lähteistä:

1. [C++ String substr() - CPP Reference](https://en.cppreference.com/w/cpp/string/basic_string/substr)
2. [C++ std::String Class - GeeksforGeeks](https://www.geeksforgeeks.org/c-string-class-and-its-application/)
3. [Laiska leikkaus - Wikipedia](https://fi.wikipedia.org/wiki/Laiska_leikkaus)