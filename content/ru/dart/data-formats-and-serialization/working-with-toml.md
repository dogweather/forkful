---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:38.166857-07:00
description: "TOML, \u0438\u043B\u0438 Tom's Obvious, Minimal Language (\u041F\u0440\
  \u043E\u0441\u0442\u043E\u0439 \u0438 \u041C\u0438\u043D\u0438\u043C\u0430\u043B\
  \u0438\u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\u0437\u044B\u043A \u0422\
  \u043E\u043C\u0430), \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\
  \u0435\u0442 \u0441\u043E\u0431\u043E\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0444\u0430\u0439\u043B\u0430 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\
  \u0430\u0446\u0438\u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043B\u0435\
  \u0433\u043A\u043E \u0447\u0438\u0442\u0430\u0435\u0442\u0441\u044F\u2026"
lastmod: '2024-03-13T22:44:44.554728-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0438\u043B\u0438 Tom's Obvious, Minimal Language (\u041F\u0440\u043E\
  \u0441\u0442\u043E\u0439 \u0438 \u041C\u0438\u043D\u0438\u043C\u0430\u043B\u0438\
  \u0441\u0442\u0438\u0447\u043D\u044B\u0439 \u042F\u0437\u044B\u043A \u0422\u043E\
  \u043C\u0430), \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0435\
  \u0442 \u0441\u043E\u0431\u043E\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u0444\
  \u0430\u0439\u043B\u0430 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\
  \u0438\u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043B\u0435\u0433\u043A\
  \u043E \u0447\u0438\u0442\u0430\u0435\u0442\u0441\u044F \u0431\u043B\u0430\u0433\
  \u043E\u0434\u0430\u0440\u044F \u0441\u0432\u043E\u0435\u0439 \u044F\u0441\u043D\
  \u043E\u0439 \u0441\u0435\u043C\u0430\u043D\u0442\u0438\u043A\u0435."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
weight: 39
---

## Как использовать:
Dart не включает встроенную поддержку для TOML, но вы можете работать с файлами TOML, используя сторонние пакеты, такие как `toml`. Сначала добавьте `toml` в ваш `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Чтение TOML
Для чтения файла TOML допустим, у вас есть простой файл конфигурации `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Вы можете разобрать этот файл TOML в Dart следующим образом:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Выводит секцию 'database'
}
```

Это выведет:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Запись TOML
Чтобы создать контент TOML, используйте `TomlBuilder`, предоставляемый пакетом `toml`:

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

Это сгенерирует и выведет строковое представление содержимого TOML, очень похожее на наш файл `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Эти примеры показывают, как читать из файла TOML и писать в него, делая работу с данными конфигурации в ваших приложениях Dart простой.
