---
title:                "C++: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Vaihtoehtoisen tekstin löytäminen ja vaihtaminen on tärkeä osa ohjelmoinnin prosessia, koska se säästää aikaa ja vaivaa. Sen avulla voit nopeasti korjata tiettyjä merkkijonoja tai muuttaa kokonaisten tiedostojen sisältöä.

## Kuinka

Yksi tapa etsiä ja korvata tekstiä C++:lla on käyttää `find` ja `replace` -funktioita. Seuraavassa esimerkissä korvaamme kaikki "koira" sanan "kissa" "teksti.txt" tiedostossa:

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main()
{
    string teksti;
    ifstream tiedosto("teksti.txt");
    string etsi = "koira";
    string korvaa = "kissa";

    while (getline(tiedosto, teksti))
    {
        size_t kohta = teksti.find(etsi);
        if (kohta != string::npos)
        {
            teksti.replace(kohta, etsi.length(), korvaa);
        }
        cout << teksti << endl;
    }
    tiedosto.close();

    return 0;
}
```

Esimerkin tuloste olisi:

```
Tänään näin kissan puistossa.
Kissa haukkuu puistossa.
Minulla on kaksi kissaa kotona.
```

## Syvempi sukellus

`find` ja `replace` -funktiot toimivat etsimällä ja korvaamalla vain yhden merkkijonon kerrallaan. Jos haluat korvata useita merkkijonoja kerralla, voit käyttää `stringstream` -luokkaa ja `while` -silmukkaa. Tässä esimerkissä korvaamme sekä "koira" sanan että "kissa" sanan "eläin" sanalla:

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

using namespace std;

int main()
{
    string teksti;
    ifstream tiedosto("teksti.txt");
    string etsi[] = {"koira", "kissa"};
    string korvaa[] = {"eläin", "eläin"};
    stringstream ss;

    while (getline(tiedosto, teksti))
    {
        for (int i = 0; i < 2; i++)
        {
            size_t kohta = teksti.find(etsi[i]);
            if (kohta != string::npos)
            {
                teksti.replace(kohta, etsi[i].length(), korvaa[i]);
            }
        }
        cout << teksti << endl;
    }
    tiedosto.close();

    return 0;
}
```

Esimerkin tuloste olisi sama kuin aiemmin: 

```
Tänään näin eläimen puistossa.
Eläin haukkuu puistossa.
Minulla on kaksi eläintä kotona.
```

## Katso myös

- [CPPReference: std::basic_string::find](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [CPPReference: std::basic_string::replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)