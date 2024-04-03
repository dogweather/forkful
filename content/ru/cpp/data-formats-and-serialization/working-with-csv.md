---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:21.589469-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \ \u043A\u043E\u0434\u0430, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0447\u0438\
  \u0442\u0430\u0435\u0442 \u0444\u0430\u0439\u043B CSV \u0438 \u043F\u0435\u0447\u0430\
  \u0442\u0430\u0435\u0442 \u0435\u0433\u043E \u0441\u043E\u0434\u0435\u0440\u0436\
  \u0438\u043C\u043E\u0435."
lastmod: '2024-03-13T22:44:45.647678-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442 \u043A\
  \u043E\u0434\u0430, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0447\u0438\u0442\
  \u0430\u0435\u0442 \u0444\u0430\u0439\u043B CSV \u0438 \u043F\u0435\u0447\u0430\u0442\
  \u0430\u0435\u0442 \u0435\u0433\u043E \u0441\u043E\u0434\u0435\u0440\u0436\u0438\
  \u043C\u043E\u0435."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
