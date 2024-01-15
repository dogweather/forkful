---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "C++: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon pieniksi kirjaimiksi? Siinä tapauksessa kun haluat vertailla merkkijonoja tai etsiä tiettyä merkkijonoa, on hyödyllistä että kaikki kirjaimet ovat samassa muodossa. Myös käyttäjäystävällisyyden kannalta on parempi, että kirjaimet näkyvät kaikki samoilla isoilla ja pienillä kirjaimilla.

## Miten

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    string s = "TÄMÄ ON MERKKIJONO";
    // muutetaan merkkijono pieniksi kirjaimiksi
    transform(s.begin(), s.end(), s.begin(), ::tolower);
    cout << s << endl;
    // tuotuun: tämä on merkkijono
    return 0;
}
```

Kuten edellä olevassa esimerkissä nähdään, merkkijnon muuttaminen pieniksi kirjaimiksi onnistuu ```transform()``` funktiolla, joka käy läpi koko merkkijonon ja muuttaa kirjaimet pieniksi. Tämän jälkeen koko uusi merkkijono tulostetaan näytölle. Tärkeää on myös huomata, että ```transform()``` funktio tarvitsee kolme argumenttia: ensimmäinen argumentti on alue johon merkkijono tallennetaan, toinen argumentti on alue josta merkkijono tulee luetaan ja kolmas argumentti on funktio, joka määrittää miten kirjaimet muutetaan.

## Syvempi sukellus

Merkit voivat olla suhteellisen monimutkaisia, ja suuressa osassa kulttuureja on erilaisia merkkejä. Siksi kansainvälinen standardi Unicode kehitettiin käsittelemään kaikenlaisia merkkejä. Tämän vuoksi C++:sää käsittelee luetun tekstin tavallisesti UTF-8 merkkikoodauksen avulla.

UTF-8-koodaus määrittää, miten merkit tallennetaan bittijonoon. Jokaisella merkillä on oma merkkikoodi, joka tallennetaan bittijonoon yhdistelemällä eri määriä bittieitä. Useimmat ASCII-merkit, jotka ovat kirjaimia ja numeroita, edustavat yhtä tavallista bittiä. Sen sijaan latinalaiset kirjaimet ja muut kansainväliset merkit edustavat useampia bittieitä.

C++ sisältää vakion nimeltä ```std::locale::global(std::locale(""))``` tämän muunnoksen tekemiseksi ja  ```std::toupper()```, ```std::tolower()``` ja ```std::isupper()``` funktiot. Vaikuttaa siltä että niillä on mahdollisuus muuntaa merkki kerrallaan, sovelluskehittäjien kohteena on järjestysmäärä muuntaa merkkijonon kerralla.

## Katso lisää

- [C++ string transform](http://www.cplusplus.com/reference/string/string/transform/)
- [Unicode ja UTF-8](http://www.utf8-ansi.com/)
- [C++ std::toupper and std::tolower function](https://www.geeksforgeeks.org/cpp-toupper-tolower-functions/)
- [std::isupper function](http://www.cplusplus.com/reference/cctype/isupper/)