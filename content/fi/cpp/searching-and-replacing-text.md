---
title:                "Tekstin etsiminen ja korvaaminen"
date:                  2024-01-20T17:57:08.775759-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tekstin etsiminen ja korvaaminen on tekstipätkien vaihtamista. Ohjelmoijat käyttävät sitä korjatakseen bugeja, päivittääkseen koodia tai muokatakseen dataa.

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