---
title:                "Робота з CSV"
aliases:
- uk/cpp/working-with-csv.md
date:                  2024-02-03T19:19:36.210589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Робота з файлами CSV (Comma Separated Values — значення, розділені комами) полягає у обробці та маніпулюванні даними, які зберігаються у простому текстовому форматі, де кожен рядок тексту представляє рядок у таблиці, а коми розділяють окремі стовпчики. Програмісти використовують це для імпорту, експорту та управління даними між різними системами через широке прийняття CSV як легкого, зрозумілого для людини формату обміну даними.

## Як це зробити:

### Читання файлу CSV за допомогою стандартної бібліотеки C++:

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
        
        // Тут обробляємо parsedRow
        for (const auto& val : parsedRow) {
            std::cout << val << "\t";
        }
        std::cout << std::endl;
    }
    
    return 0;
}
```

### Запис у файл CSV:

```cpp
#include <fstream>
#include <vector>

int main() {
    std::ofstream file("output.csv");
    std::vector<std::vector<std::string>> data = {
        {"Name", "Age", "City"},
        {"John Doe", "29", "New York"},
        {"Jane Smith", "34", "Los Angeles"}
    };
    
    for (const auto& row : data) {
        for (size_t i = 0; i < row.size(); i++) {
            file << row[i];
            if (i < row.size() - 1) file << ",";
        }
        file << "\n";
    }
    
    return 0;
}
```

### Використання сторонньої бібліотеки: `csv2`:

Хоча стандартна бібліотека C++ надає базові засоби для роботи з файлами та рядками, використання сторонніх бібліотек може спростити обробку CSV. Однією з таких бібліотек є `csv2`, відома своєю простотою у використанні та ефективністю.

- Встановлення: Зазвичай встановлюється за допомогою менеджерів пакунків, як-от Conan, або безпосередньо з GitHub репозиторію.

Приклад використання `csv2` для читання файлу CSV:

```cpp
#include <csv2/reader.hpp>
#include <iostream>

int main() {
    csv2::Reader<csv2::delimiter<','>, csv2::quote_character<'"'>, csv2::first_row_is_header<true>> csv;
    if (csv.mmap("data.csv")) {
        const auto header = csv.header();
        for (const auto row : csv) {
            for (const auto cell : row) {
                std::cout << cell.second << "\t"; // Виводимо значення кожної комірки
            }
            std::cout << std::endl;
        }
    }
    return 0;
}
```

Зразок виводу для операцій читання може виглядати так (припускаючи, що це простий триколонковий файл CSV):

```
John    29    New York    
Jane    34    Los Angeles
```

Ці приклади покликані охопити основні операції з файлами CSV у C++. Для більш складних сценаріїв, таких як робота з великими файлами або складні трансформації даних, може бути виправданим подальше дослідження спеціалізованих бібліотек або інструментів.
