---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:14.658410-07:00
description: "TOML, o Tom's Obvious, Minimal Language, \xE8 un formato di file di\
  \ configurazione facile da leggere grazie alla sua chiara semantica. I programmatori\
  \ lo\u2026"
lastmod: '2024-03-13T22:44:43.157708-06:00'
model: gpt-4-0125-preview
summary: "TOML, o Tom's Obvious, Minimal Language, \xE8 un formato di file di configurazione\
  \ facile da leggere grazie alla sua chiara semantica."
title: Lavorare con TOML
weight: 39
---

## Come fare:
Dart non include supporto incorporato per TOML, ma è possibile lavorare con i file TOML utilizzando pacchetti di terze parti come `toml`. Prima, aggiungi `toml` al tuo `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Leggere TOML
Per leggere un file TOML, supponiamo che tu abbia un semplice file di configurazione `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Puoi analizzare questo file TOML in Dart come segue:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Stampa la sezione 'database'
}
```

Questo stampa:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Scrivere TOML
Per creare contenuto TOML, utilizza il `TomlBuilder` fornito dal pacchetto `toml`:

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

Questo genererà e stamperà una rappresentazione sotto forma di stringa del contenuto TOML, molto simile al nostro file `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Questi esempi mostrano come leggere e scrivere file TOML, rendendo semplice lavorare con i dati di configurazione nelle tue applicazioni Dart.
