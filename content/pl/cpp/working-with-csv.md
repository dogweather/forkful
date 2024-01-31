---
title:                "Praca z plikami CSV"
date:                  2024-01-19
simple_title:         "Praca z plikami CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Praca z plikami CSV polega na odczycie, analizie i zapisie danych w formacie wartości rozdzielanych przecinkami - jest to uniwersalny format danych. Programiści używają go, bo CSV jest prosty, rozpowszechniony i łatwo się go przetwarza.

## How to: (Jak to zrobić?)
```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

int main() {
    std::string line;
    std::ifstream file("dane.csv");

    if (file.is_open()) {
        while (getline(file, line)) {
            std::stringstream ss(line);
            std::vector<std::string> row;
            std::string data;

            while (getline(ss, data, ',')) {
                row.push_back(data);
            }

            for (const auto& field : row) {
                std::cout << field << ' ';
            }
            std::cout << '\n';
        }
        file.close();
    } else {
        std::cout << "Nie można otworzyć pliku." << std::endl;
    }

    return 0;
}
```
Wynik działania programu zależy od zawartości `dane.csv`, ale każda linia pliku zostanie rozdzielona na pojedyncze wartości i wyświetlona w konsoli.

## Deep Dive (Dogłębna analiza)
CSV (Comma-Separated Values) zyskał na popularności wraz z rozwojem aplikacji biurowych. Alternatywami dla CSV są m.in. JSON i XML, które są bardziej elastyczne, ale też bardziej złożone. Przetwarzanie CSV w C++ jest proste, aczkolwiek nie ma dedykowanej biblioteki standardowej, co często prowadzi do pisania własnych parserów lub korzystania z bibliotek zewnętrznych, takich jak 'libcsv' czy 'Boost Tokenizer'.

## See Also (Zobacz również)
- [cppreference.com](https://cppreference.com) - Źródło informacji o funkcjach i bibliotekach C++.
- [RFC 4180](https://tools.ietf.org/html/rfc4180) - Dokumentacja standardu CSV.
- [Boost C++ Libraries](https://www.boost.org/doc/libs/?view=categorized) - Zestaw bibliotek C++, w tym Boost Tokenizer, który ułatwia pracę z tekstem.
