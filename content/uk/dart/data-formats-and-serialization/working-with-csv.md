---
title:                "Робота з CSV"
date:                  2024-03-08T21:57:53.446836-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
