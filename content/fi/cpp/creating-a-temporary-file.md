---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Väliaikaisten tiedostojen luonti C++:ssa 

## Mitä & Miksi?
Väliaikaisen tiedoston luonti on prosessi, jossa luodaan tilapäinen tiedosto tallentamaan väliaikaisia ​​tietoja. Ohjelmoijat tekevät tämän päästäkseen käsiksi tarvittaessa näihin tiedostoihin tai kun haluavat varastoida suuria tietomääriä hetkellisesti.

## Näin teet:
```C++
#include <fstream>
#include <cstdio>

// Luodaan väliaikainen tiedosto
char tempFileName[L_tmpnam] {};
std::tmpnam(tempFileName);

std::ofstream file(tempFileName);
// Kirjoita tiedostoon
file << "Hello, World!";
file.close();

std::ifstream read_file(tempFileName);
std::string line;
getline(read_file, line);
std::cout << line << '\n';  // te tulostaa: "Hello, World!"
```

## Syvälle Sukellus
(1) Historiallinen yhteys: Väliaikaisten tiedostojen luonti ei ole uusi käsite. Se on ollut olemassa ja käytetty lähes yhtä kauan kuin tietokoneohjelmointi.

(2) Vaihtoehdot: Voit käyttää `std::tmpfile` -funktiota väliaikaisen tiedoston luomiseen, mutta se luo binaarimuotoisen tiedoston, eikä sitä suositella käytettäväksi, koska sen hallinta ja poistaminen voivat olla monimutkaisia.

(3) Toteutuksen yksityiskohdat: C++ Standard Kirjasto tarjoaa `tmpnam` -funktion väliaikaisen tiedoston nimen luomiseen ja `std::ofstream` tiedostoon kirjoittamiseen. Tiedoston nimi on yleensä ainutlaatuinen, jotta voidaan välttää ristiriidat nykyisten tiedostojen nimien kanssa.

## Katso myös:
- [C++ Documentation: tmpnam](http://www.cplusplus.com/reference/cstdio/tmpnam/)
- [C++ Documentation: ofstream](http://www.cplusplus.com/reference/fstream/ofstream/)
- [StackOverflow: How to Create a Temporary File](https://stackoverflow.com/questions/15126802/how-to-create-a-temporary-file-correctly-with-c-and-boost)
- [StackOverflow: Best Practice When Creating Temporary Files](https://stackoverflow.com/questions/230062/whats-the-best-way-to-create-a-temp-file-in-java)