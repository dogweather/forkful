---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:14.991767-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u043E\u0431\u0440\u0430\u0431\u0430\
  \u0442\u044B\u0432\u0430\u0442\u044C \u0444\u0430\u0439\u043B\u044B CSV \u0432 Dart,\
  \ \u043E\u0431\u044B\u0447\u043D\u043E \u043B\u0438\u0431\u043E \u0432\u0440\u0443\
  \u0447\u043D\u0443\u044E \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\
  \u044E\u0442 \u0442\u0435\u043A\u0441\u0442, \u043B\u0438\u0431\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0438\u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438\
  \ \u0434\u043B\u044F \u0443\u043F\u0440\u043E\u0449\u0435\u043D\u0438\u044F \u0437\
  \u0430\u0434\u0430\u0447\u0438.\u2026"
lastmod: '2024-03-13T22:44:44.553099-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043E\u0431\u0440\u0430\u0431\u0430\u0442\
  \u044B\u0432\u0430\u0442\u044C \u0444\u0430\u0439\u043B\u044B CSV \u0432 Dart, \u043E\
  \u0431\u044B\u0447\u043D\u043E \u043B\u0438\u0431\u043E \u0432\u0440\u0443\u0447\
  \u043D\u0443\u044E \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u044E\
  \u0442 \u0442\u0435\u043A\u0441\u0442, \u043B\u0438\u0431\u043E \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\
  \u0438\u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 \u0434\
  \u043B\u044F \u0443\u043F\u0440\u043E\u0449\u0435\u043D\u0438\u044F \u0437\u0430\
  \u0434\u0430\u0447\u0438."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

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
