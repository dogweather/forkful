---
title:                "Merkkijonon kirjoittaminen isolla alkukirjaimella"
html_title:           "C++: Merkkijonon kirjoittaminen isolla alkukirjaimella"
simple_title:         "Merkkijonon kirjoittaminen isolla alkukirjaimella"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

### Miksi haluaisit muuntaa merkkijonon alkukirjaimet isoihin kirjaimiin?

Merkkijonon alkukirjainten muuntaminen isoihin tai pieniin kirjaimiin on yleinen ohjelmointitoiminto, jota käytetään muun muassa käyttäjän syöttämien tietojen validointiin ja tietojen jäsennystä varten. Se voi myös helpottaa tietojen vertailua, kun kirjaimien koko ei vaikuta lopputulokseen.

## Kuinka

### Kuinka muuntaa merkkijonon alkukirjaimet isoihin kirjaimiin käyttäen C++:aa

Useimmissa ohjelmointikielissä on valmiita funktioita, jotka hoitavat merkkijonon muuntamisen isoonsa tai pieneen muotoon. C++:ssa voit käyttää `toupper()`-funktiota, joka muuntaa yhden merkin isoksi kirjaimeksi. Alla olevassa koodiesimerkissä käytämme `toupper()`-funktiota ja `for`-silmukkaa käydäksemme läpi koko merkkijonon:

```C++
#include <iostream>
#include <string>
#include <cctype>

using namespace std;

int main()
{
    string s = "tervetuloa käyttämään C++:aa";
    
    for (int i = 0; i < s.length(); i++)
    {
        s[i] = toupper(s[i]);
    }
    
    cout << s << endl;
    
    return 0;
}

// Output:
// TERVETULOA KÄYTTÄMÄÄN C++:AA
```

## Syvempi sukellus

### Syvällisempää tietoa merkkijonon muuntamisesta isoihin kirjaimiin

C++:n `toupper()`-funktio hyödyntää C-kielen `cctype`-kirjaston funktioita. Tämä tarkoittaa, että `toupper()`-funktio käyttää ASCII-taulukkoa määrittääkseen, mikä merkin iso muoto on. Jos haluat käyttää Unicode-merkkejä, sinun on käytettävä `towupper()`-funktiota, joka hyödyntää `cwctype`-kirjastoa.

On myös mahdollista käyttää `transform()`-funktiota yhdistelmänä funktioiden `toupper()` tai `tolower()` kanssa, jos haluat muuttaa kaikki merkit isiksi tai pieniksi kerralla.

## Katso myös
- [C++ string manipulation functions (C++ merkkijonofunktiot)](https://www.educba.com/c-plus-plus-string-functions/) 
- [C++ toupper() function (C++ toupper()-funktio)](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)