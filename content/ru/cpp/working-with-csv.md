---
title:                "Работа с CSV"
date:                  2024-01-29T00:04:21.589469-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/working-with-csv.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Работа с CSV (Comma-Separated Values, значения, разделённые запятыми) подразумевает взаимодействие с простыми текстовыми файлами, которые хранят табличные данные. Программисты используют CSV из-за его простоты и совместимости в различных системах, что идеально подходит для обмена данными между разным программным обеспечением.

## Как это сделать:

Вот фрагмент кода, который читает файл CSV и печатает его содержимое.

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

int main() {
    std::string line, cell;
    std::vector<std::vector<std::string>> csvData;
    std::ifstream file("example.csv");

    while (std::getline(file, line)) {
        std::stringstream lineStream(line);
        std::vector<std::string> rowData;
        
        while (std::getline(lineStream, cell, ',')) {
            rowData.push_back(cell);
        }
        csvData.push_back(rowData);
    }
    
    for (const auto& row : csvData) {
        for (const auto& col : row) {
            std::cout << col << " ";  // В зависимости от структуры вашего CSV, подстройте разделитель.
        }
        std::cout << std::endl;
    }
    return 0;
}
```

Пример вывода для CSV, содержащего имена и возраст:
```
John 25
Jane 28
```

## Подробнее

CSV существует с начала 1970-х годов. Это основной формат для простого экспорта и импорта данных, но он не подходит для сложных иерархических данных, с которыми лучше справляются XML и JSON. C++ не имеет встроенной поддержки CSV, но работа с файлами и строками достаточно проста. Вы работаете со стандартным вводом/выводом и манипуляцией со строками, не забывая следить за особыми случаями, такими как кавычки и запятые внутри ячеек. Библиотеки типа `libcsv` и `Boost.Tokenizer` могут упростить задачу, если вы имеете дело с более сложными файлами CSV.

## Смотрите также

- [RFC 4180](https://tools.ietf.org/html/rfc4180), общий формат и MIME-тип для файлов CSV.
- [Справочник C++ для ввода/вывода](http://www.cplusplus.com/reference/fstream/)
- [Библиотеки Boost C++](https://www.boost.org/)
- [10 минут до pandas - работа с CSV в Python для сравнения](https://pandas.pydata.org/pandas-docs/stable/user_guide/10min.html)
