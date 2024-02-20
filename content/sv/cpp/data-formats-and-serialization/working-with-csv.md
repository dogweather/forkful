---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:12.731212-07:00
description: "Att arbeta med CSV-filer (v\xE4rden separerade med kommatecken) handlar\
  \ om att bearbeta och manipulera data lagrad i ett enkelt textformat, d\xE4r varje\
  \ rad i\u2026"
lastmod: 2024-02-19 22:04:57.470223
model: gpt-4-0125-preview
summary: "Att arbeta med CSV-filer (v\xE4rden separerade med kommatecken) handlar\
  \ om att bearbeta och manipulera data lagrad i ett enkelt textformat, d\xE4r varje\
  \ rad i\u2026"
title: Arbeta med CSV
---

{{< edit_this_page >}}

## Vad & Varför?

Att arbeta med CSV-filer (värden separerade med kommatecken) handlar om att bearbeta och manipulera data lagrad i ett enkelt textformat, där varje rad i texten representerar en rad i en tabell, och kommatecken separerar individuella kolumner. Programmerare använder detta för att importera, exportera och hantera data över olika system på grund av CSV:s breda acceptans som ett lättviktigt, människo-läsabart datautbytesformat.

## Hur man gör:

### Läsa en CSV-fil med hjälp av C++-standardsbiblioteket:

```cpp
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

int main() {
    std::ifstream file("data.csv");
    std::string line;
    
    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::string cell;
        std::vector<std::string> parsedRow;
        
        while (std::getline(lineStream, cell, ',')) {
            parsedRow.push_back(cell);
        }
        
        // Processa parsedRow här
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Skriva till en CSV-fil:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Namn", "Ålder", "Stad"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& rad : data) {
        for (size_t i = 0; i < rad.size(); i++) {
            file << rad[i];
            if (i < rad.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Använda ett tredjepartsbibliotek: `csv2`:

Medan C++-standardsbiblioteket tillhandahåller grundläggande verktyg för att arbeta med filer och strängar, kan användning av tredjepartsbibliotek förenkla bearbetningen av CSV. Ett sådant bibliotek är `csv2`, känt för sin användarvänlighet och effektivitet.

- Installation: Installeras vanligtvis via pakethanterare som Conan eller direkt från dess GitHub-repositorium.

Exempel som använder `csv2` för att läsa en CSV-fil:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto rad : csv) {
            for (const auto cell : rad) {
                std::cout << cell.second << "\t"; // Skriv ut varje cellvärde
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Exempel på utmatning för läsoperationer kan se ut så här (med antagandet av en enkel CSV-fil med tre kolumner):

```
John    29    New York    
Jane    34    Los Angeles
```

Dessa exempel syftar till att täcka grundläggande CSV-operationer i C++. För mer komplexa scenarier, som att hantera stora filer eller komplexa datatransformationer, kan vidare utforskning av specialiserade bibliotek eller verktyg vara motiverat.
