---
title:                "Tiedostotekstin lukeminen"
html_title:           "C++: Tiedostotekstin lukeminen"
simple_title:         "Tiedostotekstin lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Miksi siis avata ja lukea tekstitiedostoja C++:lla? Se on kätevä tapa lukea ja käsitellä suuria määriä dataa, kuten käyttäjän syötteitä tai tiedostoja verkosta.

## Kuinka
Tässä esimerkissä näytämme, kuinka lukea ja tulostaa tiedoston sisältö käyttäen C++:aa.

```C++
#include <iostream>
#include <fstream>

int main() {
  // Avataan tiedosto objectiksi
  std::ifstream tiedosto("esimerkki.txt"); 
  
  // Tarkistetaan, että tiedosto on avattu
  if (tiedosto.is_open()) { 
    
    // Luetaan ja tulostetaan jokainen rivi tiedostosta
    std::string rivi;
    while (std::getline(tiedosto, rivi)) { 
      std::cout << rivi << std::endl; 
    }
    
    // Suljetaan tiedosto, kun ollaan luettu kaikki rivit
    tiedosto.close();
  }
  
  return 0;
}
```

Seuraava koodin pohjalta luotu esimerkkitekstitiedosto "esimerkki.txt" sisältää seuraavan tekstin:

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
Ja tämä on kolmas.
```

Kun koodi suoritetaan, tulostuu seuraava:

```
Tämä on ensimmäinen rivi.
Tämä on toinen rivi.
Ja tämä on kolmas.
```

## Syvällisempi sukellus
Fstream-kirjasto sisältää monia eri metodeja tiedostojen lukemiseen ja käsittelemiseen. Yksi hyödyllinen metodi on "peek()", joka palauttaa seuraavan merkin tiedostossa ilman sen lukemista. Tämä on hyödyllistä esimerkiksi tiedoston käsittelyssä rivien välistä tyhjän rivin löytämiseen.

## Katso myös
- [C++ tiedostojen käsittely (w3schools)](https://www.w3schools.com/cpp/cpp_files.asp)
- [Fstream referenssi (cplusplus.com)](http://www.cplusplus.com/reference/fstream/)