---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Regular lausekkeet eli regex ovat kaavoja, joilla suodatetaan ja käsitellään tekstidataa yhteensopivuuden mukaan. Ne ovat välttämättömyys, kun on selvitettävä tai muokattava suuria tekstiaineistoja – nopeutta ja tarkkuutta haluttaessa.

## How to: (Miten tehdään:)
```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    // Alustetaan regex
    regex sana_regex("(\\w+)\\s(C\\+\\+)");
    
    // Käsiteltävä merkkijono
    string lause = "Opi C++ 24 tunnissa!";
    
    // Regex-haku
    smatch tulokset;
    if (regex_search(lause, tulokset, sana_regex)) {
        cout << "Löydetty lause: " << tulokset[0] << '\n'; // Koko lause
        cout << "Eka sana: " << tulokset[1] << '\n'; // Ensimmäinen ryhmä (\w+)
    }
    
    return 0;
}
```

Sample output:
```
Löydetty lause: Opi C++
Eka sana: Opi
```

## Deep Dive (Syventävä tieto):
Historiallisesti regex-syntaksi on polveutunut alun perin 1950-luvulla kehitetyistä matemaattisista notaatioista. C++:ssa <regex>-kirjasto tarjoaa regex-tuen, mutta voit myös käyttää POSIX- tai Boost-kirjastoja. Itse regex-käsittely on usein kallista prosessoriajalle, joten harkitse vaihtoehtoja tai optimointia suorituskyvyn kannalta kriittisissä sovelluksissa.

## See Also (Katso myös):
- C++ <regex>: http://www.cplusplus.com/reference/regex/
- Boost.Regex library: https://www.boost.org/doc/libs/release/libs/regex/
- Modern C++ regex tutorial: https://en.cppreference.com/w/cpp/regex