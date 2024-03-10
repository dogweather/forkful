---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:49.702473-07:00
description: "YAML, skr\xF3t od YAML Ain't Markup Language, to format serializacji\
  \ danych, kt\xF3ry jest czytelny dla cz\u0142owieka. Programi\u015Bci u\u017Cywaj\u0105\
  \ go do plik\xF3w\u2026"
lastmod: '2024-03-09T21:05:59.847253-07:00'
model: gpt-4-0125-preview
summary: "YAML, skr\xF3t od YAML Ain't Markup Language, to format serializacji danych,\
  \ kt\xF3ry jest czytelny dla cz\u0142owieka. Programi\u015Bci u\u017Cywaj\u0105\
  \ go do plik\xF3w\u2026"
title: Praca z YAML
---

{{< edit_this_page >}}

## Co i dlaczego?

YAML, skrót od YAML Ain't Markup Language, to format serializacji danych, który jest czytelny dla człowieka. Programiści używają go do plików konfiguracyjnych, wymiany danych oraz w aplikacjach, gdzie dane muszą być przechowywane lub przesyłane w formacie łatwym do zrozumienia.

## Jak to zrobić:

W Dart, praca z YAML zazwyczaj wymaga użycia biblioteki stron trzecich, ponieważ język nie zawiera wbudowanych możliwości parsowania YAML. Popularnym wyborem jest pakiet `yaml`. Aby zacząć, musisz dodać ten pakiet do swojego `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Pamiętaj, aby uruchomić `pub get`, aby pobrać pakiet.

### Czytanie YAML

Aby odczytać plik YAML, najpierw zaimportuj pakiet `yaml`, a następnie użyj funkcji `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Wyjście: John Doe
}

```

Zakładając, że twój plik `config.yaml` wygląda tak:

```yaml
name: John Doe
age: 30
```

### Pisanie YAML

Chociaż pakiet `yaml` jest świetny do parsowania, nie obsługuje pisania w YAML. W tym celu być może będziesz musiał ręcznie przekonwertować swoje dane na YAML lub użyć innego pakietu, jeśli jest dostępny. Lub, bardziej bezpośrednio, zarządzać transformacjami danych i wypisywać je jako ciągi znaków, które pasują do składni YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Wyjście: name: Jane Doe
                             //         age: 29
}
```

Jest to podstawowe podejście i może nie być odpowiednie dla złożonych struktur danych lub specjalnych funkcji YAML. Dla zaawansowanych potrzeb być może będziesz musiał poszukać lub przyczynić się do bardziej kompleksowego pakietu Dart.
