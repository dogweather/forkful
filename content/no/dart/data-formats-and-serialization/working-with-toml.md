---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:57.022186-07:00
description: "Hvordan: Dart inkluderer ikke innebygd st\xF8tte for TOML, men du kan\
  \ jobbe med TOML-filer ved \xE5 bruke tredjepartspakker som `toml`. F\xF8rst, legg\
  \ til `toml` i\u2026"
lastmod: '2024-03-13T22:44:40.513327-06:00'
model: gpt-4-0125-preview
summary: "Dart inkluderer ikke innebygd st\xF8tte for TOML, men du kan jobbe med TOML-filer\
  \ ved \xE5 bruke tredjepartspakker som `toml`."
title: "\xC5 Arbeide med TOML"
weight: 39
---

## Hvordan:
Dart inkluderer ikke innebygd støtte for TOML, men du kan jobbe med TOML-filer ved å bruke tredjepartspakker som `toml`. Først, legg til `toml` i din `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### Lese TOML
For å lese en TOML-fil, la oss anta at du har en enkel konfigurasjonsfil `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Du kan parse denne TOML-filen i Dart slik:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var innhold = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(innhold);
  var data = doc.toMap();

  print(data['database']); // Skriv ut 'database'-seksjonen
}
```

Dette skriver ut:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### Skrive TOML
For å opprette TOML-innhold, bruk `TomlBuilder` som tilbys av `toml`-pakken:

```dart
import 'package:toml/toml.dart';

void main() {
  final bygger = TomlBuilder();

  bygger.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = bygger.build().toString();
  print(tomlString);
}
```

Dette vil generere og skrive ut en strengrepresentasjon av TOML-innholdet, veldig lik vår `config.toml`-fil:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Disse eksemplene viser hvordan man leser fra og skriver til TOML-filer, noe som gjør det enkelt å jobbe med konfigurasjonsdata i dine Dart-applikasjoner.
