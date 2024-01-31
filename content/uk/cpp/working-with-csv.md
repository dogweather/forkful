---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Що і чому?
Робота з CSV полягає в читанні, записі, і маніпуляції даними у форматі, що їх легко читати як людині, так і машині. Програмісти використовують CSV через його простоту та сумісність з таблицями електронних таблиць.

## Як це робити:
```C++
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

// Функція для читання CSV-файлу
std::vector<std::vector<std::string>> readCSV(const std::string& filename) {
    std::vector<std::vector<std::string>> data;
    std::ifstream file(filename);
    std::string line;
    while (getline(file, line)) {
        std::stringstream linestream(line);
        std::string cell;
        std::vector<std::string> rowData;
        while (getline(linestream, cell, ',')) {
            rowData.push_back(cell);
        }
        data.push_back(rowData);
    }
    return data;
}

// Функція для запису даних у CSV-файл
void writeCSV(const std::string& filename, const std::vector<std::vector<std::string>>& data) {
    std::ofstream file(filename);
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); ++i) {
            file << row[i];
            if (i < row.size() - 1) {
                file << ",";
            }
        }
        file << "\n";
    }
}

int main() {
    // Запис даних у CSV
    std::vector<std::vector<std::string>> myData = {
        {"Name", "Age", "City"},
        {"Alex", "31", "Kyiv"},
        {"Danylo", "29", "Lviv"}
    };
    
    writeCSV("example.csv", myData);
    
    // Читання даних з CSV
    auto readData = readCSV("example.csv");
    for (const auto& row : readData) {
        for (const auto& cell : row) {
            std::cout << cell << " ";
        }
        std::cout << std::endl;
    }

    return 0;
}
```

**Вивід:**
```
Name Age City
Alex 31 Kyiv
Danylo 29 Lviv
```

## Глибоке занурення
CSV (Comma-Separated Values) — це простий формат обміну даними, який був створений у ранні 70-ті. Є альтернативи, такі як XML та JSON, що надають більше можливостей для серіалізації складних даних. Працювати з CSV у C++ можна без зовнішніх бібліотек, але для великих чи складних файлів є бібліотеки, такі як 'csv-parser' і 'Boost CSV'.

## Див. також
- [RFC 4180](https://tools.ietf.org/html/rfc4180), стандарт CSV.
- [libcsv](http://sourceforge.net/projects/libcsv/), бібліотека C для читання і запису CSV файлів.
- [Boost Library](https://www.boost.org/), збірка бібліотек для C++, які містять компонент для роботи з CSV.
