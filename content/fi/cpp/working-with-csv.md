---
title:                "C++: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi käyttää CSV-tiedostoja ohjelmoitaessa?

CSV-tiedostot ovat yksi yleisimmistä tietorakenteista, joita käytetään tietojen tallentamiseen ja jakamiseen ohjelmoinnissa. Ne ovat erityisen käteviä, kun haluat tallentaa taulukkomaisia tietoja, kuten esimerkiksi käyttäjätietoja tai tuotehintoja. CSV-tiedostot ovat myös yhteensopivia monien ohjelmointikielten kanssa, ja ne ovat helppo tapa tallentaa ja käsitellä suuria määriä tietoa.

## Miten käytät CSV-tiedostoja C++:ssa

CSV-tiedostoja voidaan käsitellä helposti C++:ssa käyttämällä <iostream> ja <fstream>-kirjastoja. Seuraavassa esimerkissä luodaan uusi CSV-tiedosto ja tallennetaan siihen käyttäjätietoja:

```C++
#include <iostream>
#include <fstream>

int main()
{
  // Luodaan uusi tiedosto
  std::ofstream file("tiedosto.csv");

  // Tallennetaan käyttäjätiedot tiedostoon
  file << "Käyttäjänimi, Käyttäjän sähköposti, Käyttäjän ikä \n";
  file << "Matti, matti@matti.fi, 25 \n";
  file << "Maija, maija@maija.fi, 30 \n";

  // Suljetaan tiedosto
  file.close();

  std::cout << "CSV-tiedosto on luotu ja tiedot tallennettu." << std::endl;
  return 0;
}
```

Tämän esimerkin tuloksena syntyy uusi tiedosto nimeltä "tiedosto.csv" ja sen sisältö näyttää tältä:

```
Käyttäjänimi, Käyttäjän sähköposti, Käyttäjän ikä
Matti, matti@matti.fi, 25
Maija, maija@maija.fi, 30
```

## Syvempi sukellus CSV-tiedostojen kanssa työskentelyyn

CSV-tiedostojen kanssa työskentelyyn on hyvä tutustua tarkemmin, jos haluat tallentaa monimutkaisempia tietorakenteita tai käsitellä suuria määriä tietoa. Joitain hyödyllisiä asioita, joita kannattaa oppia, ovat esimerkiksi tiedostojen lukeminen ja kirjoittaminen rivi kerrallaan, tietojen eri kenttien erottaminen pilkulla ja tiedoston hakeminen ja muokkaaminen.

Tässä on joitain linkkejä, jotka voivat auttaa sinua aloittamaan:

- [C++ CSV-kirjasto](https://github.com/vincentlaucsb/csv-parser)
- [CSV-tiedostojen käsittely C++:ssa](https://www.geeksforgeeks.org/writing-csv-files-in-cpp/)
- [CSV-tiedostojen lukeminen ja kirjoittaminen C++:ssa](https://www.fluentcpp.com/2017/04/21/how-to-read-a-csv-file-in-cpp/)

# Katso myös

- [Miten tallentaa ja käsitellä tietoja C++:ssa?](https://postinumerot.fi/coding/2/tallennetaan-ja-käsitellään-tietoja)
- [C++:n perusasiat aloittelijoille](https://www.codecogs.com/learn/tutorials/c-plus-plus/)