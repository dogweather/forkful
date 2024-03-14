---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:33.102558-07:00
description: "TOML oder Toms offensichtliche, minimale Sprache, ist ein Konfigurationsdateiformat,\
  \ das aufgrund seiner klaren Semantik leicht zu lesen ist.\u2026"
lastmod: '2024-03-13T22:44:53.606847-06:00'
model: gpt-4-0125-preview
summary: "TOML oder Toms offensichtliche, minimale Sprache, ist ein Konfigurationsdateiformat,\
  \ das aufgrund seiner klaren Semantik leicht zu lesen ist.\u2026"
title: Arbeiten mit TOML
---

{{< edit_this_page >}}

## Was & Warum?

TOML oder Toms offensichtliche, minimale Sprache, ist ein Konfigurationsdateiformat, das aufgrund seiner klaren Semantik leicht zu lesen ist. Programmierer nutzen es für die Konfiguration von Softwareanwendungen, weil es einfach zu parsen ist und minimale Verwirrung oder Fehler erzeugt.

## Wie geht das:

Dart bietet keine integrierte Unterstützung für TOML, aber Sie können mit TOML-Dateien arbeiten, indem Sie Drittanbieterpakete wie `toml` verwenden. Fügen Sie zunächst `toml` zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML lesen

Um eine TOML-Datei zu lesen, nehmen wir an, Sie haben eine einfache Konfigurationsdatei `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Sie können diese TOML-Datei in Dart wie folgt parsen:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Druckt den Abschnitt 'database'
}
```

Das druckt:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML schreiben

Um TOML-Inhalte zu erstellen, verwenden Sie den von dem `toml`-Paket bereitgestellten `TomlBuilder`:

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

Das generiert und druckt eine Zeichenfolgendarstellung des TOML-Inhalts, die der unserer `config.toml`-Datei sehr ähnlich ist:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Diese Beispiele zeigen, wie Sie in TOML-Dateien lesen und schreiben können, was es einfach macht, mit Konfigurationsdaten in Ihren Dart-Anwendungen zu arbeiten.
