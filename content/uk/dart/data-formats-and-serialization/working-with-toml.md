---
title:                "Робота з TOML"
date:                  2024-03-08T21:57:46.853683-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Що і чому?

TOML, або Мова Тома – очевидна, мінімалістична, – це формат файлу конфігурації, який легко читати завдяки своїй чіткій семантиці. Програмісти використовують його для налаштування програмних додатків, оскільки він простий у розборі і мінімізує плутанину або помилки.

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
