---
title:    "C++: Väliaikaisen tiedoston luominen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Monissa C++ ohjelmissa on tarve luoda väliaikaisia tiedostoja, jotka ovat näkyvissä vain ohjelman suorituksen ajan ja jotka poistuvat automaattisesti sen jälkeen. Tämä voi olla hyödyllistä esimerkiksi väliaikaisten tietojen tallentamiseen tai uuden tiedoston luomiseen ennen sen pysyvän tallentamista.

## Miten

```C++
#include <iostream>
#include <fstream>
#include <cstdlib>

using namespace std;

int main()
{
  // Luodaan väliaikainen tiedosto nimeltä "temp.txt"
  const char* tempFileName = "temp.txt";
  
  // Avataan tiedosto luettavaksi ja kirjoitettavaksi
  ofstream tempFile(tempFileName);
  
  // Tarkistetaan, että tiedoston avaaminen onnistui
  if (!tempFile)
  {
    cout << "Virhe: tiedoston avaaminen epäonnistui." << endl;
    exit(1);
  }
  
  // Kirjoitetaan tiedostoon merkkijono
  tempFile << "Tämä on väliaikainen tiedosto!" << endl;
  
  // Suljetaan tiedosto
  tempFile.close();
  
  // Tiedosto poistuu automaattisesti ohjelman loputtua
  
  return 0;
}
```

Tässä esimerkissä näemme, miten voimme luoda ja käyttää väliaikaista tiedostoa C++ ohjelmassa. Voimme käyttää tiedostovirtoja (fstream) lukeaksemme ja kirjoittaaksemme tiedostoon ja tiedoston sulkeutuessa tiedosto poistuu automaattisesti.

## Syvempää tietoa

Väliaikaiset tiedostot luodaan yleensä käyttäen `tmpnam()` tai `mkstemp()` funktioita. Väliaikaisen tiedoston nimeä ei kuitenkaan kannata luottaa liikaa, sillä se voi olla alttiina tietoturvaongelmille. Siksi on suositeltavaa käyttää C++17 standardissa esiteltyä `filesystem` kirjastoa, joka tarjoaa turvallisemman ja helpomman tavan luoda väliaikaisia tiedostoja.

## Katso myös

- [C++ std::tmpnam](https://en.cppreference.com/w/cpp/io/tmpnam)
- [C++ std::mkstemp](https://en.cppreference.com/w/cpp/io/c/mkstemp)
- [C++ std::filesystem::temp_directory_path](https://en.cppreference.com/w/cpp/filesystem/temp_directory_path)