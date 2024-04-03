---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:27.328391-07:00
description: "TOML, of Tom's Obvious, Minimal Language, is een configuratiebestandsformaat\
  \ dat eenvoudig te lezen is vanwege zijn duidelijke semantiek. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.530388-06:00'
model: gpt-4-0125-preview
summary: TOML, of Tom's Obvious, Minimal Language, is een configuratiebestandsformaat
  dat eenvoudig te lezen is vanwege zijn duidelijke semantiek.
title: Werken met TOML
weight: 39
---

## Wat & Waarom?

TOML, of Tom's Obvious, Minimal Language, is een configuratiebestandsformaat dat eenvoudig te lezen is vanwege zijn duidelijke semantiek. Programmeurs gebruiken het voor het configureren van softwaretoepassingen omdat het gemakkelijk te parsen is en minimale verwarring of fouten genereert.

## Hoe te:

Dart bevat geen ingebouwde ondersteuning voor TOML, maar je kunt met TOML-bestanden werken met behulp van externe pakketten zoals `toml`. Voeg eerst `toml` toe aan je `pubspec.yaml`:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML lezen

Om een TOML-bestand te lezen, laten we aannemen dat je een eenvoudig configuratiebestand `config.toml` hebt:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Je kunt dit TOML-bestand in Dart als volgt parsen:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // Print de 'database' sectie
}
```

Dit print:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML schrijven

Om TOML-content te maken, gebruik je de `TomlBuilder` die wordt geleverd door het `toml` pakket:

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

Dit genereert en print een string-representatie van de TOML-content, heel vergelijkbaar met ons `config.toml` bestand:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

Deze voorbeelden laten zien hoe je TOML-bestanden kunt lezen en schrijven, wat het eenvoudig maakt om met configuratiegegevens in je Dart-applicaties te werken.
