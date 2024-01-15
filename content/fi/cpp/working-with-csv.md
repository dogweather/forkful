---
title:                "Työskentely csv:n kanssa"
html_title:           "C++: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Miksi

CSV-tiedostot (Comma-Separated Values) ovat yksi yleisimmistä tiedostomuodoista, joita käytetään tietojen tallentamisessa ja jakamisessa. CSV-tiedostot ovat helppolukuisia ja helppokäyttöisiä, mikä tekee niistä ihanteellisia monien erilaisten ohjelmointitehtävien kannalta.

## Kuinka

CSV-tiedoston lukeminen ja kirjoittaminen C++:ssa on hyvin yksinkertaista. Aloita luomalla olio, joka edustaa CSV-tiedostoa ja määrittele sen sarakkeiden erotin merkiksi pilkku. Voit sitten käyttää `getline()` -funktiota lukeaksesi rivin tiedostosta ja `stringstream` -luokkaa jakamaan sen erillisiin sarakkeisiin. Tämän jälkeen voit tallentaa sarakkeiden arvot haluamaasi muuttujaan ja käsitellä niitä edelleen. Katso alla oleva esimerkki:

```C++
#include <iostream>
#include <fstream>
#include <sstream>

int main() {
  // Luo olio CSV-tiedostoa varten
  std::ifstream csvfile("tiedosto.csv");
  // Aseta reunamerkki pilkkua vastaavaksi
  csvfile.separator(',');
  std::string rivi;

  // Lue rivi ja tallenna se muuttujaan 
  while (getline(csvfile, rivi)) {
    // Käytä stringstream-luokkaa jakamaan rivi erillisiin sarakkeisiin
    std::stringstream ss(rivi);
    // Tallenna sarakkeiden arvot muuttujiin
    std::string sarakkeet[3]; // Oletetaan, että tiedostossa on 3 saraketta
    for (int i = 0; i < 3; i++) {
      getline(ss, sarakkeet[i], ',');
    }
    // Käsittele sarakkeiden arvot tarpeen mukaan
    // Esim. tulosta ne konsoliin
    std::cout << sarakkeet[0] << " " << sarakkeet[1] << " " << sarakkeet[2] << std::endl;
  }

  // Sulje tiedosto
  csvfile.close();
  return 0;
}
```

Tämä esimerkki lukee rivi kerrallaan CSV-tiedostoa ja tallentaa sarakkeiden arvot muuttujiin. Sen jälkeen voit käsitellä sarakkeiden arvoja tarpeen mukaan. 

## Syventävä sukellus

CSV-tiedostoissa on monia erilaisia formaatteja ja käytäntöjä, joten on tärkeää olla tietoinen erilaisista tilanteista, jotka voivat vaikuttaa tiedostojen käsittelyyn. Esimerkiksi, jos sarakkeissa on tekstiä, joka sisältää pilkkua, se voi aiheuttaa ongelmia `getline()` -funktion kanssa, sillä se käyttää pilkkua erottimena. Tässä tapauksessa kannattaa harkita käyttämään jotain muuta merkkiä, kuten puolipistettä, sarakkeiden erotinmerkkinä. 

On myös tärkeää huomata, että CSV-tiedostojen kanssa työskentelyssä voi ilmetä virheitä ja muotoiluongelmia, varsinkin jos tiedosto sisältää paljon tietoa. Siksi on tärkeää testata koodia huolellisesti ja käsitellä mahdolliset virheet asianmukaisesti.

## Katso myös

- [C++ -tiedoston lukeminen ja kirjoittaminen](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [CppCSV - ohjelmakirjasto CSV-tiedostojen lukuun ja kirjoitukseen C++:ssa](https://github.com/vincentlaucsb/CppCSV)