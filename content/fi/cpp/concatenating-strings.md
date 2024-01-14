---
title:                "C++: Joustenavien merkkijonojen yhdistäminen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on tärkeä osa ohjelmointia, sillä se mahdollistaa eri palasien yhdistämisen yhdeksi kokonaisuudeksi. Tämä on erityisen hyödyllistä, kun halutaan luoda dynaamisia viestejä tai käsitellä muuttuvia tietoja.

## Kuinka

Stringien yhdistäminen onnistuu C++ -ohjelmointikielellä käyttämällä + operaattoria. Esimerkiksi:

```C++
std::string nimi = "Matti";
std:string tervehdys = "Hei " + nimi;
std::cout << tervehdys; // tulostaa "Hei Matti"
```

Tässä esimerkissä ensimmäisellä rivillä luodaan muuttuja nimelle ja annetaan sille arvoksi "Matti". Toisella rivillä luodaan muuttuja tervehdykselle ja yhdistetään siihen edellisellä rivillä luotu nimi käyttäen + operaattoria. Lopuksi tulostetaan tervehdys STDIN-rajapinnan avulla.

Toinen tapa yhdistää stringejä on käyttää std::stringstream -luokkaa ja sen append() -funktiota. Esimerkiksi:

```C++
#include <sstream>
// ...
std::stringstream ss;
ss << "Tilisi numero on " << 123456789;
std::string lopputulos = ss.str();
std::cout << lopputulos; // tulostaa "Tilisi numero on 123456789"
```

Tässä esimerkissä luodaan ensin stringstream objekti ja sen jälkeen käytetään sen append() -funktiota lisäämään halutut stringit siihen. Lopuksi stringstreamin sisältö muunnetaan string-muotoon ja tulostetaan STDIN-rajapinnan avulla.

## Syvemmälle

Stringien yhdistämisessä on hyvä huomioida muutamia asioita. Ensinnäkin, yhdistämiseen käytettävät stringit kannattaa tarkistaa ennen yhdistämistä, jotta vältyttäisiin mahdollisilta virheiltä. Lisäksi muista, että yhdistämisen jälkeen alkuperäiset stringit eivät muutu, vaan yhdistetty tulos tallennetaan uuteen muuttujaan.

Myös muistinhallinta on tärkeää, sillä liian suurten stringien yhdistäminen saattaa aiheuttaa ohjelman kaatumisen. Tästä syystä on tärkeää tarkistaa tarvittaessa stringien kapasiteetti ja tehdä tarvittaessa muistiallokointi ennen yhdistämistä.

## Katso myös

- C++ -kielen virallisilta sivuilta löytyy lisätietoa stringien käsittelystä: https://isocpp.org/wiki/faq/strings
- Stringien yhdistämiseen liittyviä vinkkejä löytyy myös Stack Overflow -sivustolta: https://stackoverflow.com/questions/1760045/c-stdstring-concatenation