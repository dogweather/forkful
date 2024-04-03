---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:53.446836-07:00
description: "\u042F\u043A: \u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438\
  \ \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\u0438 CSV \u0443 Dart \u0432\u0438\
  \ \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0430\u0431\u043E \u043E\u0431\
  \u0440\u043E\u0431\u043B\u044F\u0454\u0442\u0435 \u0442\u0435\u043A\u0441\u0442\
  \ \u0432\u0440\u0443\u0447\u043D\u0443, \u0430\u0431\u043E \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u0435 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  \ \u0434\u043B\u044F \u0441\u043F\u0440\u043E\u0449\u0435\u043D\u043D\u044F \u0437\
  \u0430\u0432\u0434\u0430\u043D\u043D\u044F. \u0422\u0443\u0442 \u043C\u0438\u2026"
lastmod: '2024-03-13T22:44:48.837961-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 \u0444\u0430\
  \u0439\u043B\u0430\u043C\u0438 CSV \u0443 Dart \u0432\u0438 \u0437\u0430\u0437\u0432\
  \u0438\u0447\u0430\u0439 \u0430\u0431\u043E \u043E\u0431\u0440\u043E\u0431\u043B\
  \u044F\u0454\u0442\u0435 \u0442\u0435\u043A\u0441\u0442 \u0432\u0440\u0443\u0447\
  \u043D\u0443, \u0430\u0431\u043E \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0454\u0442\u0435 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438 \u0434\u043B\u044F\
  \ \u0441\u043F\u0440\u043E\u0449\u0435\u043D\u043D\u044F \u0437\u0430\u0432\u0434\
  \u0430\u043D\u043D\u044F."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

## Як:
Для роботи з файлами CSV у Dart ви зазвичай або обробляєте текст вручну, або використовуєте сторонні бібліотеки для спрощення завдання. Тут ми розглянемо обидва підходи.

### Ручний розбір CSV
Якщо ваші вимоги прості, ви можете вибрати ручний розбір рядка CSV. Це можна досягти за допомогою основних функцій маніпуляції рядками у Dart:

```dart
void main() {
  // Зразок даних CSV
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Розбір даних CSV на рядки
  List<String> lines = csvData.split('\n');
  
  // Розбір кожного рядка
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
  
  // Виведення розібраних даних
  print(data);
}

// Приклад виводу:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Використання сторонньої бібліотеки: `csv`
Для більш складних сценаріїв або для спрощення вашого коду, ви можете використовувати популярну сторонню бібліотеку, таку як `csv`. Насамперед, додайте її до свого проекту, включивши `csv: ^5.0.0` (або останню версію) у ваш файл `pubspec.yaml` у розділі `dependencies`. Потім використовуйте її наступним чином:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Використання CsvToListConverter для розбору даних CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Перший елемент списку містить заголовки
  List<String> заголовки = listData.first.map((item) => item.toString()).toList();
  
  // Вилучення рядка з заголовками перед подальшою обробкою
  listData.removeAt(0);
  
  // Конвертація в List<Map<String, dynamic>> для більш структурованого формату
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < заголовки.length; i++) {
      map[заголовки[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Виведення оброблених даних
  print(mappedData);
}

// Приклад виводу:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Обидва методи демонструють, як працювати з даними CSV: перший ручним способом, для навчальних цілей або при роботі з дуже простими структурами CSV; другий, використовуючи потужну бібліотеку, яка спрощує розбір і може обробити різні складнощі форматування CSV.
