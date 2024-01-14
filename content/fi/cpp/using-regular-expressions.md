---
title:                "C++: Säännöllisten lausekkeiden käyttö"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita C++-ohjelmoinnissa?

Säännölliset lausekkeet ovat hyödyllinen työkalu, joka mahdollistaa tietojen tarkan haun ja käsittelyn ohjelmissa. Ne ovat erityisen hyödyllisiä datan validoinnissa ja hakemisessa suuresta datamäärästä.

## Miten käyttää säännöllisiä lausekkeita C++:ssa?

Säännölliset lausekkeet voidaan ottaa käyttöön C++:ssa käyttämällä <code>regex</code>-kirjastoa. Alla on esimerkki, joka etsii merkkijonosta kaikki sanat, jotka alkavat kirjaimella "h" ja päättyvät kirjaimella "n". 

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string s = "Hei on vain yksi maailmassa oleva henkilö, jolle voi luottaa: sinä.";
    regex pattern ("h[a-zA-Z]*n");
    smatch matches;
    
    while(regex_search(s, matches, pattern)) {
        cout << matches[0] << endl;
        s = matches.suffix().str();
    }
    return 0;
}
```

Tulostaa:

```
Hei
henkilö
```

## Syvemmälle säännöllisiin lausekkeisiin

Säännöllisillä lausekkeilla on monia erilaisia käyttötarkoituksia, jotka kannattaa tutkia tarkemmin. Niiden avulla voidaan esimerkiksi suodattaa tietoja, muuntaa merkkijonoja ja suorittaa monimutkaisia hakutoimintoja. Säännöllisten lausekkeiden opiskelu auttaa myös ymmärtämään paremmin merkkijonoihin liittyviä käsitteitä kuten erikoismerkkejä ja tiedostonimien muotoilua.

## Katso myös

- [C++ regex-opetusohjelma](https://www.tutorialspoint.com/cpp_standard_library/regex.htm)
- [Säännölliset lausekkeet C++:ssa - virallinen dokumentaatio](https://en.cppreference.com/w/cpp/regex)
- [RegExr - verkkosivusto säännöllisten lausekkeiden testaamiseen ja opiskeluun](https://regexr.com/)