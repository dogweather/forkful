---
date: 2024-01-20 17:57:08.775759-07:00
description: "How to: (Kuinka:) Vanhat C++-versiot vaativat manuaalista silmukointia\
  \ tekstinkorjaukselle. Uudemmat standardit, kuten C++11 eteenp\xE4in, tuovat lambda-\u2026"
lastmod: '2024-04-05T21:53:58.428147-06:00'
model: gpt-4-1106-preview
summary: (Kuinka:) Vanhat C++-versiot vaativat manuaalista silmukointia tekstinkorjaukselle.
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to: (Kuinka:)
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "Hello, world! Hello, Finland!";
    std::string to_search = "Hello";
    std::string replace_with = "Hei";

    // Replace all occurrences of 'to_search' with 'replace_with'
    size_t pos = text.find(to_search);
    while(pos != std::string::npos) {
        text.replace(pos, to_search.length(), replace_with);
        pos = text.find(to_search, pos + replace_with.length());
    }

    std::cout << text << std::endl; // Output: "Hei, world! Hei, Finland!"

    return 0;
}
```

## Deep Dive (Syvä Sukellus)
Vanhat C++-versiot vaativat manuaalista silmukointia tekstinkorjaukselle. Uudemmat standardit, kuten C++11 eteenpäin, tuovat lambda-funktiot ja std::regex, mikä helpottaa tehtävää huomattavasti. Vaihtoehtoja on monia: voi käyttää vanhempaa `char*`-pohjaista käsittelyä, suosittua Boost-kirjastoa tai moderneja string-käsittelymetodeja. Tärkeää on valita tapa, joka on sekä tehokas että sopiva ongelmaan.

## See Also (Katso Myös)
- C++ string handling: http://www.cplusplus.com/reference/string/string/
- Regular expressions in C++: http://www.cplusplus.com/reference/regex/
- Boost String Algorithms Library: https://www.boost.org/doc/libs/release/libs/algorithm/string/
