---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Stringien muuttaminen pieniksi kirjaimiksi C++ :ssa – mitä ja miksi?

## Mitä & Miksi?
Stringin muuttaminen pieniksi kirjaimiksi uidessa me vain muutamme jokaisen kirjaimen stringissä pieneksi kirjaimeksi. Se on hyödyllistä, kun haluamme vertailla stringejä riippumatta siitä, onko niitä kirjoitettu isolla vai pienellä.

## Miten:
```C++
#include <algorithm>
#include <cctype>
#include <iostream>

int main() {
    std::string s = "Moi C++ Koodarit";
    std::transform(s.begin(), s.end(), s.begin(), 
        [](unsigned char c){ return std::tolower(c); });

    std::cout << s << std::endl;
    return 0;
}
```
Koodiin ajaminen tuottaa seuraavan tulostuksen:
```
moi c++ koodarit
```

## Deep Dive:
Alkuperäinen C++ ei tue suoraan stringejä. Mutta, C++98: n std::string-luokka tekee sen helpoksi työskennellä stringeillä. Me käytämme transform-toimintoa, jonka avulla voimme muuttaa jokaista merkkiä stringissä. Muut ratkaisut, kuten for-loopit tässä tapauksessa, eivät sovi vikkelästä ketterään koodaus tyyliin.

On arvokasta huomata, että std::tolower on vain varten merkkijonoja ASCII-alphabetissa. Muut kielet, kuten suomalainen, joka sisältää kirjaimia "ä", "ö" ja "å", voivat tarvita enemmän huolellista käsittelyä. Jos tarvitset tukea monille kielille, harkitse locale-toiminnot.

Lisäksi huomaat, että käytämme transform-toimintoa lamdda-rakenne kanssa. Tämä on vain yksi tapa tehdä se. Kuitenkin, C++ antaa meille mahdollisuuden valita omat tapamme - kunhan se toimii!

## Katso myös:
- C++ referenssi std::transform: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ referenssi std::tolower: https://en.cppreference.com/w/cpp/string/byte/tolower
- Channel9 video ASCII ja unicode: https://channel9.msdn.com/Events/GoingNative/2013/Keynote-Bjarne-Stroustrup-GoingNative-2013