---
date: 2024-01-20 17:53:58.185761-07:00
description: "How to: \"N\xE4in teet:\"."
lastmod: '2024-04-05T21:53:58.460764-06:00'
model: gpt-4-1106-preview
summary: ''
title: Tekstitiedoston lukeminen
weight: 22
---

## How to:
"Näin teet:"
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("esimerkki.txt"); // Avaa tiedosto
    std::string line;
    
    if(file.is_open()) {
        while(std::getline(file, line)) {
            std::cout << line << std::endl; // Tulostaa jokaisen rivin
        }
        file.close(); // Sulje tiedosto
    } else {
        std::cerr << "Tiedoston avaaminen epäonnistui." << std::endl;
    }
    
    return 0;
}
```
Outputti saattaa näyttää tältä, jos `esimerkki.txt`-tiedosto sisältää muutaman rivin tekstiä:
```
Hei, tämä on teksti esimerkkitiedostosta.
Toinen rivi tekstiä.
```

## Deep Dive:
"Sukellus Syvemmälle":
C++ on tukenut tekstitiedostojen käsittelyä alusta asti. `<fstream>`-kirjastoa on käytetty tiedostojen käsittelyyn jo 90-luvulta lähtien, vaikka C++-standardi saattaa saada päivityksiä, perusperiaatteet pysyvät samoina. Vaihtoehtoisia tapoja lukea tiedostoja ovat esimerkiksi C-tyylinen FILE*-käsittely tai uudempi C++17-standardiin lisätty `<filesystem>`-moduuli. Erilaiset kirjastot ja funktiot tarjoavat lisäominaisuuksia, kuten tiedoston lukeminen muistiin ilman välitystiedostoja tai rinnakkainen tiedostonkäsittely.

## See Also:
"Katso Myöskin":
- C++ standardikirjasto: https://en.cppreference.com/w/
- Tiedostonkäsittelyn alternatiivit: https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm
- C++17 FileSystem Library: https://en.cppreference.com/w/cpp/filesystem
