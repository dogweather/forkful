---
title:                "Säännöllisten lausekkeiden käyttö"
aliases:
- fi/cpp/using-regular-expressions.md
date:                  2024-02-03T19:16:12.920059-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
C++:n säännölliset lausekkeet ovat merkkijonoja, jotka määrittelevät hakukaavan, ja niitä käytetään merkkijonojen vastaavuuksien etsintään tai manipulointiin. Ohjelmoijat käyttävät niitä tehtäviin, kuten syötteen validointiin, esiintymien etsimiseen merkkijonoista tai merkkijonojen pilkkomiseen tokeniksi, mikä tekee niistä korvaamattoman työkalun tehokkaaseen ja vaikuttavaan tekstin käsittelyyn.

## Kuinka:
C++11 toi tuen säännöllisille lausekkeille standardikirjastoon, `<regex>`, tarjoten robustin kehyksen merkkijonojen etsintään ja manipulointiin. Tässä on perusesimerkki säännöllisten lausekkeiden käytöstä mallin etsimiseen merkkijonosta:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "Sähköposti löytyi!" << std::endl;
    } else {
        std::cout << "Sähköpostia ei löytynyt." << std::endl;
    }

    return 0;
}
```
**Esimerkkituloste**
```
Sähköposti löytyi!
```

Monimutkaisempia manipulaatioita varten, kuten mallien korvaaminen merkkijonoissa, C++:n säännölliset lausekkeet voivat olla hyvin käteviä:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "Sade Espanjassa putoaa lähinnä tasangolle.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**Esimerkkituloste**
```
S*d* Esp*nj*ss* p*t** l*h*nn* t*s*ng*ll*.
```

Ohjelmoijille, jotka tutkivat standardikirjaston ulkopuolella, Boost Regex -kirjasto (`boost/regex.hpp`) on suosittu kolmannen osapuolen vaihtoehto, joka tarjoaa parannettuja regex-ominaisuuksia ja suorituskykyoptimointeja, erityisesti monimutkaisten mallien tai laajojen datan käsittelyjen osalta:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost-kirjastot ovat hauskoja!";
    boost::regex expr("(\\w+)\\s(kirjastot)"); // Vastaa "Boost-kirjastot"
    std::string fmt("GNU \\1"); // Korvaa "GNU Boost"

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**Esimerkkituloste**
```
GNU Boost ovat hauskoja!
```

Nämä esimerkit raapaisevat vain pintaa C++:n kyvyistä säännöllisten lausekkeiden kanssa, havainnollistaen perusetsintöjä, mallien vastaavuuksia ja korvauksia, joko käyttäen standardikirjastoa tai tehostettuna Boostin tehokkaalla regex-toteutuksella.
