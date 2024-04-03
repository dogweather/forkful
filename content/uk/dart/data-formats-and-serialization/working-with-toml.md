---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.853683-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Dart \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0443 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\
  \u0443 \u0434\u043B\u044F TOML, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438 \u0437\
  \ \u0444\u0430\u0439\u043B\u0430\u043C\u0438 TOML, \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\u043E\
  \u043D\u043D\u0456 \u043F\u0430\u043A\u0435\u0442\u0438, \u043D\u0430\u043F\u0440\
  \u0438\u043A\u043B\u0430\u0434, `toml`.\u2026"
lastmod: '2024-03-13T22:44:48.839631-06:00'
model: gpt-4-0125-preview
summary: "Dart \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0454 \u0432\u0431\
  \u0443\u0434\u043E\u0432\u0430\u043D\u0443 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u043A\u0443 \u0434\u043B\u044F TOML, \u0430\u043B\u0435 \u0432\u0438 \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u043F\u0440\u0430\u0446\u044E\u0432\u0430\u0442\u0438\
  \ \u0437 \u0444\u0430\u0439\u043B\u0430\u043C\u0438 TOML, \u0432\u0438\u043A\u043E\
  \u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 \u0441\u0442\u043E\u0440\
  \u043E\u043D\u043D\u0456 \u043F\u0430\u043A\u0435\u0442\u0438, \u043D\u0430\u043F\
  \u0440\u0438\u043A\u043B\u0430\u0434, `toml`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 TOML"
weight: 39
---

## Як це зробити:
Dart не включає вбудовану підтримку для TOML, але ви можете працювати з файлами TOML, використовуючи сторонні пакети, наприклад, `toml`. Спочатку додайте `toml` до вашого `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Читання TOML
Щоб прочитати файл TOML, припустімо, у вас є простий файл конфігурації `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Ви можете розібрати цей файл TOML в Dart так:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Друкує розділ 'database'
}
```

Це надрукує:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Запис TOML
Щоб створити вміст TOML, використовуйте `TomlBuilder`, наданий пакетом `toml`:

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

Це створить і надрукує рядкове представлення вмісту TOML, дуже схоже на наш файл `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Ці приклади показують, як читати з файлів TOML і записувати в них, роблячи простим роботу з даними конфігурації у ваших додатках на Dart.
