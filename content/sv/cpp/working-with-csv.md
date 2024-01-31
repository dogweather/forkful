---
title:                "Arbeta med csv"
date:                  2024-01-19
simple_title:         "Arbeta med csv"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV-filer håller data skild av kommatecken, perfekt för enkelhet och gränssnitt mellan system. Programmerare använder CSV för att enkelt utbyta och bearbeta data som tabeller och listor.

## Hur gör man:
```C++
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>

// Läs från CSV-fil och lagra i en vector av vectors
std::vector<std::vector<std::string>> lasCSV(const std::string& filnamn) {
    std::vector<std::vector<std::string>> resultat;
    std::ifstream fil(filnamn);
    std::string rad;
    
    while (std::getline(fil, rad)) {
        std::vector<std::string> kolumn;
        std::stringstream ss(rad);
        std::string falt;

        while (std::getline(ss, falt, ',')) {
            kolumn.push_back(falt);
        }
        resultat.push_back(kolumn);
    }
    return resultat;
}

// Skriv ut innehållet i din CSV-data
void skrivUtCSV(const std::vector<std::vector<std::string>>& data) {
    for (const auto& rad : data) {
        for (const auto& falt : rad) {
            std::cout << falt << " ";
        }
        std::cout << '\n';
    }
}

int main() {
    auto data = lasCSV("exempel.csv");
    skrivUtCSV(data);
}
```

Sample Output:
```
Namn Ålder Stad
Alice 29 Stockholm
Bob 34 Göteborg
```

## Fördjupning
CSV, Comma-Separated Values, existerar sedan 1970-talet och är ett mycket spritt textformat. Alternativ som JSON och XML ger mer struktur, men CSV vinner på sin enkelhet. I C++ kan man hantera CSV med standardbiblioteket, men bibliotek som `Boost` ger ännu fler verktyg.

## Se även
- [RFC 4180](https://tools.ietf.org/html/rfc4180), den formella standarden för CSV.
- [CSV parserbibliotek på GitHub](https://github.com/ben-strasser/fast-cpp-csv-parser), för mer avancerade användningsfall.
- [C++ Boost Library](https://www.boost.org/), om du vill utforska CSV hantering med Boost.
