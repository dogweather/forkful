---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Hahmojen poistaminen mallin mukaan C++:ssa tarkoittaa tiettyjä hahmoja vastaavien merkkijonojen poistamista koodista. Koodaajat tekevät tämän usein koodinsa yksinkertaistamiseksi tai sen suorituskyvyn parantamiseksi.

## Miten:

Tässä on esimerkki siitä, miten voit poistaa hahmoja, jotka täsmäävät tiettyyn malliin.

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
   std::string test = "Hello, world!";
   test.erase(std::remove(test.begin(), test.end(), 'o'), test.end());
   std::cout << test << std::endl;
   return 0;
}
```

Tämä koodipätkä poistaa kirjaimen 'o' merkkijonosta "Hello, world!". Tuloste on seuraava:

```
Hell, wrld!
```
## Syvää tietoa:

(1) Historiallinen konteksti: Hahmojen poistaminen merkkijonoista on ollut ohjelmoinnin perusominaisuus sen alkuajoista asti. C++:n STL (Standard Template Library) ja sen `remove()` funktio tarjoaa yksinkertaisen ja tehokkaan tavan tehdä tämä.

(2) Vaihtoehdot: Voit myös käyttää muokattuja for-loops tai while-loops, mutta ne ovat yleensä sekä hitaampia että monimutkaisempia.

(3) Toteutus yksityiskohtia: `remove()` funktio ei itse poista hahmoja. Se järjestää elementit niin, että ne, jotka haluat säilyttää, ovat ensin. Sitten se palauttaa osoittimen ensimmäiseen poistettavaan elementtiin. Tämän jälkeen `erase()` funktio poistaa kaikki elementit tästä kohdasta loppuun.

## Katso myös:

- Jos haluat lisätietoja `remove()` ja `erase()` funktioista, tutustu niiden dokumentaatioon cppreference.com.

    [https://en.cppreference.com/w/cpp/algorithm/remove](https://en.cppreference.com/w/cpp/algorithm/remove)
    
    [https://en.cppreference.com/w/cpp/string/basic_string/erase](https://en.cppreference.com/w/cpp/string/basic_string/erase)

- Jos haluat ymmärtää paremmin hahmojen poistamista merkkijonoista, katso tämä Stack Overflow -keskustelu:

    [https://stackoverflow.com/questions/5891610/how-to-remove-certain-characters-from-a-string-in-c](https://stackoverflow.com/questions/5891610/how-to-remove-certain-characters-from-a-string-in-c)