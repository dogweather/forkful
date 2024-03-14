---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:53.446836-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\
  \u0430\u043C\u0438 CSV (Comma Separated Values - \u0437\u043D\u0430\u0447\u0435\u043D\
  \u043D\u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\
  \u043C\u0430\u043C\u0438) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\
  \u0454 \u0440\u043E\u0437\u0431\u0456\u0440 \u0456 \u0441\u0442\u0432\u043E\u0440\
  \u0435\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0445\
  \ \u0444\u0430\u0439\u043B\u0456\u0432, \u0434\u0435 \u043A\u043E\u0436\u0435\u043D\
  \ \u0440\u044F\u0434\u043E\u043A \u043C\u0456\u0441\u0442\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:48.837961-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u0444\u0430\u0439\u043B\u0430\
  \u043C\u0438 CSV (Comma Separated Values - \u0437\u043D\u0430\u0447\u0435\u043D\u043D\
  \u044F, \u0440\u043E\u0437\u0434\u0456\u043B\u0435\u043D\u0456 \u043A\u043E\u043C\
  \u0430\u043C\u0438) \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\u0430\u0454\
  \ \u0440\u043E\u0437\u0431\u0456\u0440 \u0456 \u0441\u0442\u0432\u043E\u0440\u0435\
  \u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u0438\u0445 \u0444\
  \u0430\u0439\u043B\u0456\u0432, \u0434\u0435 \u043A\u043E\u0436\u0435\u043D \u0440\
  \u044F\u0434\u043E\u043A \u043C\u0456\u0441\u0442\u0438\u0442\u044C\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
---

{{< edit_this_page >}}

## Що і чому?

Робота з файлами CSV (Comma Separated Values - значення, розділені комами) передбачає розбір і створення текстових файлів, де кожен рядок містить значення, розділені комами. Програмісти роблять це для того, щоб забезпечити обмін даними між різними застосунками або для спрощення зберігання даних у легкодоступному, зручному для читання форматі.

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
