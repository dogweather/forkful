---
title:    "C++: Mallia vastaavien merkkien poistaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointitehtävissä saattaa syntyä tarve poistaa merkkejä tietystä kaavasta. Tämä voi johtua esimerkiksi tietojen puhdistamisesta tai tietojen käsittelystä.

## Miten

Poistaminen merkkejä, jotka vastaavat tietyssä kaavassa, voidaan suorittaa käyttämällä C ++: n sisäänrakennettuja merkkijonojen käsittelytoimintoja. Tässä on yksinkertainen koodiesimerkki, joka poistaa kaikki välimerkit annetusta merkkijonosta ja tulostaa tuloksen:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string s = "Tämä on#& esimerkkimerkkijono!";
  string s_clean = "";

  for (char c : s) {
    if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') {
      s_clean += c;
    }
  }

  cout << s_clean << endl;

  return 0;
}
```

Tämä tulostaa: "Tämäonesimerkkimerkkijono"

## Syventävä sukellus

Yllä oleva koodiesimerkki perustuu oletukselle, että haluamme poistaa välimerkit merkkijonosta. Jos haluamme poistaa kaikki numerot, voimme muuttaa ehtoa `if` lauseke seuraavasti: `c >= '0' && c <= '9'`. Sen sijaan voimme myös antaa luettelona kaikki halutut merkit poistettavaksi, esimerkiksi: `if (c == '#' || c == '&' || c == '$')`. Tällä tavalla voit mukauttaa poistettavat merkit tarpeidesi mukaan.

## Katso myös

- [Merkkijonojen käsittely C ++: lla](https://www.cplusplus.com/reference/string/string/)
- [Merkit, jotka vastaavat erilaisia ​​ehtoja](https://en.cppreference.com/w/cpp/language/ascii)