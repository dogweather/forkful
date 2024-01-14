---
title:                "C++: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

On monia erilaisia käyttötarkoituksia, joiden vuoksi henkilö haluaisi lukea tekstitiedostoa C++-ohjelmoinnilla. Ehkä he haluavat analysoida tietoja tai tallentaa niitä myöhempää käyttöä varten. Olipa kyseessä sitten mikä tahansa syy, lukeminen tekstitiedostoista on tärkeä taito kaikille C++-ohjelmoijille.

## Miten

Aloitetaan luomalla tekstiruutu, jossa on teksti sisällä. Tämän avulla voimme harjoitella tiedoston lukemista ja tulostamista konsoliin. Katso esimerkki alla olevassa koodilohkossa:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
  // Avataan tiedosto streamiin
  ifstream tiedosto("tekstitiedosto.txt");

  // Alustetaan merkkijono, johon tallennetaan tiedoston sisältö
  string sisalto;

  // Luetaan tiedosto while-silmukassa
  while (getline(tiedosto, sisalto))
  {
    // Tulostetaan sisältö konsoliin
    cout << sisalto << endl;
  }

  // Suljetaan tiedosto
  tiedosto.close();

  return 0;
}
```

Koodin suorittamisen jälkeen konsoliin tulostuu tiedoston sisältö.

```
Tämä on esimerkki
tekstiruudusta
jossa on sisältöä.
```

Koodissa käytetään `ifstream`-luokkaa, joka mahdollistaa tiedostojen lukemisen input streamina. Tämän jälkeen `getline()`-funktiolla voidaan lukea yksi rivi tiedostosta ja tallentaa se merkkijonoon. Sen jälkeen luettu rivi voidaan tulostaa konsoliin `cout`-komennolla.

## Syvempää tietoa

Jos haluat lukea tiedoston sisällön toiseen muuttujaan, esimerkiksi `char`-taulukkoon, voit käyttää `get()`-funktiota. Esimerkiksi:

```C++
// Alustetaan taulukko, johon tallennetaan tiedoston sisältö
char sisalto[100];

// Luetaan tiedostoa kunnes ollaan saavutettu sen loppu
while (!tiedosto.eof())
{
  // Luetaan yksi merkki kerrallaan ja tallennetaan se taulukkoon
  tiedosto.get(sisalto, 100, EOF);
}

// Tulostetaan sisältö konsoliin
cout << sisalto << endl;
```

Huomaatko eron `getline()`-funktiosta? `get()` lukee tiedostoa kerrallaan ja asettaa lukukohdan tiedostossa eteenpäin. Siksi seuraavan `get()`-kutsun ei tarvitse aloittaa tiedoston alusta.

## Katso myös

- [C++ - Tekstitiedostojen käsittely](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [fstream-luokan dokumentaatio](http://www.cplusplus.com/reference/fstream/fstream/)
- [Harjoituksia tiedostojen lukemiseen ja kirjoittamiseen C++:lla](https://www.w3resource.com/cpp-exercises/file-handling/index.php)