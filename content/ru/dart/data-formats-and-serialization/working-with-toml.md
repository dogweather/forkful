---
title:                "Работа с TOML"
date:                  2024-03-08T21:57:38.166857-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

TOML, или Tom's Obvious, Minimal Language (Простой и Минималистичный Язык Тома), представляет собой формат файла конфигурации, который легко читается благодаря своей ясной семантике. Программисты используют его для настройки программных приложений, поскольку он легко анализируется и вызывает минимум путаницы или ошибок.

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
