---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Stringin pituuden selvittäminen tarkoittaa merkkijonoa muodostavien merkkien määrän laskemista. Ohjelmoijat tarvitsevat tätä tietoa muun muassa sellaisten algoritmien toteuttamiseen, jotka käsittelevät tai muokkaavat tekstejä.

## Kuinka:

Voit laskea stringin pituuden C++:lla seuraavalla tavalla:

```C++
#include <iostream>
#include <string>

int main() {
    std::string teksti = "Ohjelmointi on kivaa!";
    std::cout << "Tekstin pituus on: " << teksti.length() << '\n';
    return 0;
}
```
Ohjelman tulostus näkyy seuraavasti:

``` 
Tekstin pituus on: 20
```

## Syvä sukellus:

Historiallisesti C++:n varhaisissa versioissa stringeillä ei ollut `.length()` funktiota. Sen sijaan ohjelmoijien piti käydä läpi koko stringi merkki kerrallaan kunnes ilmestyi nollamerkki, joka osoitti stringin lopun.

Vaihtoehtoisesti voit laskea stringin pituuden `size()`-funktiolla, joka toimii samalla tavalla kuin `length()`:

```C++
std::string teksti = "Ohjelmointi on kivaa!";
std::cout << "Tekstin pituus on: " << teksti.size() << '\n';
```

C++:n `.length()` ja `.size()` metodien toteutus riippuu käytettävästä standardikirjastosta ja alustasta. Suurin osa käytännössä toteuttaa ne O(1) operaationa, mikä tarkoittaa, että pituuden selvittäminen vie aina saman ajan riippumatta stringin pituudesta.

## Katso myös:

- [C++ string](http://www.cplusplus.com/reference/string/string/)
- [String length function](https://www.programiz.com/cpp-programming/library-function/cstring/strlen)
- [Difference between length() and size()](https://stackoverflow.com/questions/905479/difference-between-string-length-and-string-size-in-c)