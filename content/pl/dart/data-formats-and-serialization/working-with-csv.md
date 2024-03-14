---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:29.984930-07:00
description: "Praca z plikami CSV (Comma Separated Values - warto\u015Bci oddzielone\
  \ przecinkami) polega na przetwarzaniu i generowaniu plik\xF3w tekstowych, gdzie\
  \ ka\u017Cda linia\u2026"
lastmod: '2024-03-13T22:44:35.117262-06:00'
model: gpt-4-0125-preview
summary: "Praca z plikami CSV (Comma Separated Values - warto\u015Bci oddzielone przecinkami)\
  \ polega na przetwarzaniu i generowaniu plik\xF3w tekstowych, gdzie ka\u017Cda linia\u2026"
title: Praca z CSV
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (Comma Separated Values - wartości oddzielone przecinkami) polega na przetwarzaniu i generowaniu plików tekstowych, gdzie każda linia zawiera wartości oddzielone przecinkami. Programiści robią to, aby umożliwić wymianę danych między różnymi aplikacjami lub ułatwić przechowywanie danych w lekkim, czytelnym dla człowieka formacie.

## Jak to zrobić:

Aby obsługiwać pliki CSV w Dart, zazwyczaj przetwarzasz tekst ręcznie lub używasz bibliotek stron trzecich, aby uprościć zadanie. Tutaj przyjrzymy się obu podejściom.

### Ręczne parsowanie CSV

Jeśli twoje potrzeby są proste, możesz zdecydować się na ręczne parsowanie łańcucha CSV. Można to osiągnąć za pomocą podstawowych funkcji manipulacji łańcuchami w Dart:

```dart
void main() {
  // Przykładowe dane CSV
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Dzielenie danych CSV na linie
  List<String> lines = csvData.split('\n');
  
  // Parsowanie każdej linii
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // Wyświetlanie przeanalizowanych danych
  print(data);
}

// Przykładowe wyjście:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Używanie biblioteki stron trzecich: `csv`

W bardziej złożonych scenariuszach lub aby uprościć kod, możesz użyć popularnej biblioteki stron trzecich, takiej jak `csv`. Najpierw dodaj ją do swojego projektu, wpisując `csv: ^5.0.0` (lub najnowszą wersję) w pliku `pubspec.yaml` w sekcji `dependencies`. Następnie użyj jej w następujący sposób:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Użyj CsvToListConverter do parsowania danych CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Pierwszy element listy zawiera nagłówki
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Usunięcie wiersza z nagłówkami, zanim przejdziemy dalej
  listData.removeAt(0);
  
  // Konwersja na List<Map<String, dynamic>> dla bardziej uporządkowanego formatu
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Wyświetlanie zmapowanych danych
  print(mappedData);
}

// Przykładowe wyjście:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Obie metody demonstrują, jak pracować z danymi CSV: pierwsza ręcznie, do celów edukacyjnych lub podczas pracy z bardzo prostymi strukturami CSV; druga, wykorzystująca potężną bibliotekę, która upraszcza parsowanie i może obsługiwać różne złożoności formatowania CSV.
