---
title:                "C++: Merkkijonon muuttaminen isoin kirjaimin"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi yksi saattaa haluta muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Yksinkertaisimmillaan tämä voi olla pelkästään esteettinen valinta, mutta usein se on myös tarpeellista tietyn ohjelman tai sovelluksen toiminnan vuoksi.

## Kuinka tehdä

Merkinjonojen käsittely on perustavanlaatuinen osa ohjelmointia, ja niiden manipulointi tapahtuu monilla eri tavoilla. Tässä esittelemme yksinkertaisen esimerkin C++:lla merkkijonon ensimmäisen kirjaimen muuttamisesta isoksi:

```C++
#include <iostream>
#include <string>

using namespace std;

int main()
{
    string s = "esimerkki";
    
    // Muutetaan ensimmäinen kirjain isoksi
    s[0] = toupper(s[0]);
    
    // Tulostetaan muokattu merkkijono
    cout << s << endl;
    
    return 0;
}
```

Tämä ohjelma tulostaa "Esimerkki", mikäli ohjelmointikielesi on suunniteltu tuottamaan isoja kirjaimia. Jos käytät esimerkiksi Cia tai Sqla, tulee tulosteeseen "ESIMERKKI".

## Syvemmälle

Tähän mennessä olemme vain muuttaneet ensimmäistä kirjainta ja tulostaneet uudelleen muokatun merkkijonon. Mutta miten tämä muutos oikein tapahtuu?

Kun luodaan merkkijono eri ohjelmointikielillä, kuten C++:lla, se tallennetaan järjestelmään tavallisesti niin, että jokainen kirjain tallennetaan jonkun numeron perusteella. Esimerkiksi kirjain "a" saattaa vastata lukua 97. Joten kun haluamme muuttaa merkkijonon kirjainta, käytännössä muutamme vain kyseisen lukuarvon.

## Katso myös

- [C++ opetusohjelma](https://www.ohjelmointiputka.net/opetusohjelmat/cpp-programmointi-opas)
- [Stringin merkkijonon korvaaminen](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Stringin taulukon käyttö](https://www.geeksforgeeks.org/c-string-find-function/)