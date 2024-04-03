---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:32.925437-07:00
description: "TOML, czyli Tom's Obvious, Minimal Language (J\u0119zyk Oczywisty i\
  \ Minimalny Toma), to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do\
  \ odczytu dzi\u0119ki\u2026"
lastmod: '2024-03-13T22:44:35.118672-06:00'
model: gpt-4-0125-preview
summary: "TOML, czyli Tom's Obvious, Minimal Language (J\u0119zyk Oczywisty i Minimalny\
  \ Toma), to format pliku konfiguracyjnego, kt\xF3ry jest \u0142atwy do odczytu dzi\u0119\
  ki jasnej semantyce."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Dart nie zawiera wbudowanego wsparcia dla TOML, ale możesz pracować z plikami TOML, używając pakietów stron trzecich, takich jak `toml`. Najpierw dodaj `toml` do swojego `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Czytanie TOML
Aby przeczytać plik TOML, załóżmy, że masz prosty plik konfiguracyjny `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Możesz przetworzyć ten plik TOML w Dart w następujący sposób:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Wydrukuj sekcję 'database'
}
```

To wydrukuje:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Pisanie TOML
Aby utworzyć zawartość TOML, użyj `TomlBuilder` dostarczonego przez pakiet `toml`:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

To wygeneruje i wydrukuje ciąg reprezentujący zawartość TOML, bardzo podobny do naszego pliku `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Te przykłady pokazują, jak czytać z plików TOML i zapisywać do nich, co ułatwia pracę z danymi konfiguracyjnymi w twoich aplikacjach Dart.
