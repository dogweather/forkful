---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:14.991767-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F, \u0440\
  \u0430\u0437\u0434\u0435\u043B\u0435\u043D\u043D\u044B\u0435 \u0437\u0430\u043F\u044F\
  \u0442\u044B\u043C\u0438) \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \ \u0441\u0435\u0431\u044F \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\u0435\
  \u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u044B\u0445 \u0444\u0430\u0439\u043B\u043E\u0432, \u0433\u0434\u0435 \u043A\u0430\
  \u0436\u0434\u0430\u044F \u0441\u0442\u0440\u043E\u043A\u0430 \u0441\u043E\u0434\
  \u0435\u0440\u0436\u0438\u0442 \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F\
  ,\u2026"
lastmod: '2024-03-11T00:14:18.151377-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F, \u0440\u0430\
  \u0437\u0434\u0435\u043B\u0435\u043D\u043D\u044B\u0435 \u0437\u0430\u043F\u044F\u0442\
  \u044B\u043C\u0438) \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\
  \u0435\u0431\u044F \u0430\u043D\u0430\u043B\u0438\u0437 \u0438 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\
  \u0445 \u0444\u0430\u0439\u043B\u043E\u0432, \u0433\u0434\u0435 \u043A\u0430\u0436\
  \u0434\u0430\u044F \u0441\u0442\u0440\u043E\u043A\u0430 \u0441\u043E\u0434\u0435\
  \u0440\u0436\u0438\u0442 \u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F,\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
---

{{< edit_this_page >}}

## Что и Почему?

Работа с файлами CSV (значения, разделенные запятыми) включает в себя анализ и генерацию текстовых файлов, где каждая строка содержит значения, разделенные запятыми. Программисты делают это для обеспечения обмена данными между различными приложениями или для облегчения хранения данных в легко читаемом формате.

## Как это сделать:

Чтобы обрабатывать файлы CSV в Dart, обычно либо вручную обрабатывают текст, либо используют сторонние библиотеки для упрощения задачи. Здесь мы рассмотрим оба подхода.

### Ручной разбор CSV

Если ваши требования просты, вы можете выбрать ручной разбор строки CSV. Это можно сделать с помощью основных функций манипулирования строками в Dart:

```dart
void main() {
  // Пример данных CSV
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Разбиение данных CSV на строки
  List<String> lines = csvData.split('\n');
  
  // Разбор каждой строки
  List<Map<String, String>> data = [];
  List<String> заголовки = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < заголовки.length; j++) {
      record[заголовки[j]] = row[j];
    }
    data.add(record);
  }
  
  // Вывод разобранных данных
  print(data);
}

// Пример вывода:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Использование сторонней библиотеки: `csv`

Для более сложных сценариев или для упрощения вашего кода вы можете использовать популярную стороннюю библиотеку, например `csv`. Сначала добавьте ее в свой проект, включив `csv: ^5.0.0` (или последнюю версию) в файл `pubspec.yaml` в разделе `dependencies`. Затем используйте ее следующим образом:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Использование CsvToListConverter для разбора данных CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Первый элемент списка содержит заголовки
  List<String> заголовки = listData.first.map((item) => item.toString()).toList();
  
  // Удаление строки с заголовками перед дальнейшей обработкой
  listData.removeAt(0);
  
  // Преобразование в List<Map<String, dynamic>> для более структурированного формата
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < заголовки.length; i++) {
      map[заголовки[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Вывод преобразованных данных
  print(mappedData);
}

// Пример вывода:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Оба метода демонстрируют, как работать с данными CSV: первый вручную, для учебных целей или при работе с очень простыми структурами CSV; второй, с использованием мощной библиотеки, которая упрощает разбор и может обрабатывать различные сложности форматирования CSV.
