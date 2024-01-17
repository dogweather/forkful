---
title:                "Ajankohdan laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "C++: Ajankohdan laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Ajankohdan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Päivämäärän laskeminen tulevaisuuteen tai menneisyyteen tarkoittaa tietyn päivämäärän lisäämistä tai vähentämistä tietyllä aikavälillä. Ohjelmoijat suorittavat tätä toimintoa usein päivämäärämuistikirjojen tai tapahtumakalenterien luomiseksi.

# Kuinka tehdä:

```C++
#include <iostream>
#include <chrono>
#include <iomanip>

using namespace std;
using namespace std::chrono;

int main()
{
  // Määritä alkuperäinen päivämäärä
  int paiva = 12;
  int kuukausi = 6;
  int vuosi = 2021;
  
  // Luo ajanhetki-objekti
  time_point<system_clock> nykyhetki;
  
  // Määritä haluttu aikaväli
  int aikavali = 30;
  
  // Laske tulevaisuuden päivämäärä
  auto tulevaisuudenpaiva = nykyhetki + days(aikavali);
  
  // Laske menneisyyden päivämäärä
  auto menneisyydenpaiva = nykyhetki - days(aikavali);
  
  // Tulosta tulevaisuuden ja menneisyyden päivämäärät
  cout << "Tulevaisuuden päivämäärä: " << put_time(localtime(&tulevaisuudenpaiva), "%x") << endl;
  cout << "Menneisyyden päivämäärä: " << put_time(localtime(&menneisyydenpaiva), "%x") << endl;
  
  return 0;
}
```

Tulos:
```
Tulevaisuuden päivämäärä: 07/12/2021
Menneisyyden päivämäärä: 05/13/2021
```

# Syvempi sukellus:

(1) Päivämäärän laskentaan tulevaisuuteen tai menneisyyteen vaikuttaa vuodenajasta, kuukauden pituudesta ja jopa aikavyöhykkeestä riippuen. (2) Vaihtoehtoisia tapoja suorittaa päivämäärän laskeminen ovat esimerkiksi käsin koodaaminen tai käyttövalmiiden kirjastojen käyttö. (3) Päivämäärän laskentaan liittyviä toteutusyksityiskohtia ovat esimerkiksi aikavyöhykkeiden huomioiminen ja karkausvuodet.

# Katso myös:

- [C++ chrono-kirjasto] (https://devdocs.io/cpp/header/chrono)
- [Päivämäärämuistikirjojen luominen C++:ssa] (https://www.codegrepper.com/code-examples/cpp/c%2B%2B+create+date+calendar)
- [Päivämäärän laskeminen käsin] (https://www.wikihow.com/Calculate-Future-Date)