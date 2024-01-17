---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "C++: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Luotaessasi väliaikaista tiedostoa luot tiedostotyypin, jolla on uniikki nimi. Koodaajat tekevät tämän useisiin tarkoituksiin, kuten tallentaakseen väliaikaisia tietoja tai kommunikoidakseen tiedostojen välillä.

## Miten:

```C++
#include <fstream>
#include <cstdio>

int main() {
  // Luo väliaikainen tiedosto
  std::ofstream tmp_file("tmp.txt");
  
  // Tarkistaa, onko tiedosto luotu
  if (tmp_file.is_open()) {
    // Kirjoita tiedostoon
    tmp_file << "Tämä on väliaikainen tiedosto!" << std::endl;
    // Sulje tiedosto
    tmp_file.close();
  }

  // Avaa ja lue tiedostosta
  std::ifstream tmp("tmp.txt");
  std::string content;
  std::getline(tmp, content);

  // Tulosta tiedoston sisältö
  std::cout << content << std::endl;
  
  // Poista tiedosto
  std::remove("tmp.txt");

  return 0; 
}
```

Esimerkkituloste:

```
Tämä on väliaikainen tiedosto!
```

## Syventyminen:

Väliaikaisten tiedostojen luominen on ollut yleinen käytäntö ohjelmoinnissa jo vuosien ajan. Nykyään se on edelleen hyödyllistä monissa käyttötapauksissa, vaikka joillakin alustoilla voi olla parempia vaihtoehtoja, kuten käyttämällä muistipuhdistusta.

Väliaikaisten tiedostojen luomista varten on olemassa myös useita ohjelmistokirjastoja, kuten standardikirjaston ```fstream``` ja ```cstdio```. Näitä kirjastoja käyttämällä voit helposti luoda, kirjoittaa, lukea ja poistaa väliaikaisia tiedostoja.

Väliaikaisten tiedostojen luomisen yhteydessä on myös tärkeää huolehtia, että niitä käytetään vastuullisesti ja että ne poistetaan, kun niitä ei enää tarvita, jotta ne eivät pysy turhaan järjestelmässä.

## Katso myös:

- [C++ fstream -kirjasto](https://en.cppreference.com/w/cpp/header/fstream)
- [C Standard Library -kirjasto](https://www.cplusplus.com/reference/cstdio/)