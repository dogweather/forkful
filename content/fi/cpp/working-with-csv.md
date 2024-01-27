---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä & Miksi?

CSV on yksinkertainen tiedostomuoto datan tallentamiseen. Ohjelmoijat käyttävät sitä, koska se on helppo lukea ja kirjoittaa sekä ihmisille että ohjelmille.

## How to:
Miten:

```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

// CSV:n lukufunktio
std::vector<std::vector<std::string>> lueCSV(const std::string& tiedosto) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(tiedosto);
    std::string rivi;

    while (std::getline(file, rivi)) {
        std::stringstream rivi_stream(rivi);
        std::string solu;
        std::vector<std::string> rivi_data;

        while (std::getline(rivi_stream, solu, ',')) {
            rivi_data.push_back(solu);
        }

        data.push_back(rivi_data);
    }

    return data;
}

// CSV:n kirjoitusfunktio
void kirjoitaCSV(const std::string& tiedosto, const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(tiedosto);
    
    for (const auto& rivi : data) {
        for (size_t i = 0; i < rivi.size(); ++i) {
            file << rivi[i];
            if (i < rivi.size() - 1) file << ",";
        }
        file << "\n";
    }
}

int main() {
    // CSV:n lukeminen
    auto data = lueCSV("esimerkki.csv");
    
    // Tulosta luettu data
    for (const auto& rivi : data) {
        for (const auto& solu : rivi) {
            std::cout << solu << " ";
        }
        std::cout << std::endl;
    }
    
    // CSV:n kirjoittaminen
    kirjoitaCSV("uusi.csv", data);
    
    return 0;
}
```

## Deep Dive
Syvä Sukellus

CSV (Comma-Separated Values) syntyi jo 1970-luvulla, ja sen formaatti on pysyt melko muuttumattomana. Vaihtoehtoja CSV:lle ovat esimerkiksi JSON ja XML, jotka tukevat monimutkaisempia rakenteita. CSV:n käsittelyssä tärkeää on tietää, että standardoitu formaatti puuttuu, mikä voi johtaa yhteensopivuusongelmiin.

## See Also
Lisätietoja

- [RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files](https://tools.ietf.org/html/rfc4180)
- [C++-kirjaston dokumentaatio](http://www.cplusplus.com/reference/)
- [Boost Library for C++](https://www.boost.org/)
