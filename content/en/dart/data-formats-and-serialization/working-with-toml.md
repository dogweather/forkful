---
date: 2024-03-08 21:33:31.340448-07:00
description: "How to: Dart doesn't include built-in support for TOML, but you can\
  \ work with TOML files using third-party packages like `toml`. First, add `toml`\
  \ to your\u2026"
lastmod: '2024-03-13T22:44:59.841260-06:00'
model: gpt-4-0125-preview
summary: Dart doesn't include built-in support for TOML, but you can work with TOML
  files using third-party packages like `toml`.
title: Working with TOML
weight: 39
---

## How to:
Dart doesn't include built-in support for TOML, but you can work with TOML files using third-party packages like `toml`. First, add `toml` to your `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Reading TOML
To read a TOML file, let's assume you have a simple configuration file `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

You can parse this TOML file in Dart as follows:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Print the 'database' section
}
```

This prints:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Writing TOML
To create TOML content, use the `TomlBuilder` provided by the `toml` package:

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

This will generate and print a string representation of the TOML content, very similar to our `config.toml` file:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

These examples show how to read from and write to TOML files, making it simple to work with configuration data in your Dart applications.
