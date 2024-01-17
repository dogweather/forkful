---
title:                "Tekstitiedoston lukeminen"
html_title:           "C++: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lue tekstitiedosto (text file) on toiminto, jossa ohjelma lukee ja käsittelee tiedostossa olevaa tekstiä. Ohjelmoijat tekevät tätä, jotta he voivat käyttää ja manipuloida tiedostojen sisältöä koodissaan.

## Kuinka:
Esimerkkejä koodista ja tulosteista käytettäessä C++ -kielen ```...``` koodilohkoja.

```c++
// Avataan tiedosto lukemista varten ja tallennetaan se muuttujaan "tiedosto"
ifstream tiedosto("tekstitiedosto.txt");

// Luetaan tiedostosta yhden rivin verran ja tallennetaan se teksti-muuttujaan
string teksti;
getline(tiedosto, teksti);

// Tulostetaan luettu teksti näytölle
cout << teksti << endl;

// Suljetaan tiedosto
tiedosto.close();

// Tulos:
// Tämä on esimerkkiteksti tiedostossa.
```

## Syvällisempi sukellus:
Tiedostojen lukemisella on pitkä historia tietokoneohjelmoinnissa ja se on edelleen tärkeä osa ohjelmistojen kehittämistä. On olemassa myös muita tapoja käsitellä tiedostojen sisältöä, kuten kirjoittaminen tai muokkaaminen. Tiedostojen luku toteutetaan usein käyttämällä syöte- ja tulostusoperaatioita tai tietokantaohjelmointia.

## Katso myös:
- [C++ getline() -funktio](https://www.cplusplus.com/reference/string/string/getline/)
- [Tiedostojen lukeminen C++:lla](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)