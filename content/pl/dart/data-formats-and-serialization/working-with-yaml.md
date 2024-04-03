---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:49.702473-07:00
description: "Jak to zrobi\u0107: W Dart, praca z YAML zazwyczaj wymaga u\u017Cycia\
  \ biblioteki stron trzecich, poniewa\u017C j\u0119zyk nie zawiera wbudowanych mo\u017C\
  liwo\u015Bci parsowania YAML.\u2026"
lastmod: '2024-03-13T22:44:35.115115-06:00'
model: gpt-4-0125-preview
summary: "W Dart, praca z YAML zazwyczaj wymaga u\u017Cycia biblioteki stron trzecich,\
  \ poniewa\u017C j\u0119zyk nie zawiera wbudowanych mo\u017Cliwo\u015Bci parsowania\
  \ YAML."
title: Praca z YAML
weight: 41
---

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
