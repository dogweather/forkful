---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:38.244940-07:00
description: "Hvordan: I Dart inneb\xE6rer arbeid med YAML vanligvis \xE5 bruke et\
  \ tredjeparts bibliotek ettersom spr\xE5ket ikke inkluderer innebygd YAML-tolkingskapasitet.\
  \ Et\u2026"
lastmod: '2024-03-13T22:44:40.509899-06:00'
model: gpt-4-0125-preview
summary: "I Dart inneb\xE6rer arbeid med YAML vanligvis \xE5 bruke et tredjeparts\
  \ bibliotek ettersom spr\xE5ket ikke inkluderer innebygd YAML-tolkingskapasitet."
title: Arbeide med YAML
weight: 41
---

## Hvordan:
I Dart innebærer arbeid med YAML vanligvis å bruke et tredjeparts bibliotek ettersom språket ikke inkluderer innebygd YAML-tolkingskapasitet. Et populært valg er `yaml`-pakken. For å starte, må du legge til denne pakken i din `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Husk å kjøre `pub get` for å hente pakken.

### Lese YAML
For å lese en YAML-fil, importer først `yaml`-pakken og deretter bruk `loadYaml`-funksjonen:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Utdata: John Doe
}

```

Antatt at din `config.yaml`-fil ser slik ut:

```yaml
name: John Doe
age: 30
```

### Skrive YAML
Selv om `yaml`-pakken er flott for tolking, støtter den ikke skriving av YAML. For det, kan du måtte konvertere dataene dine til YAML manuelt eller bruke en annen pakke hvis tilgjengelig. Eller, mer rettfram, håndtere dine datatransformasjoner og utgi dem som strenger som matcher YAML-syntaksen:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Utdata: name: Jane Doe
                             //         age: 29
}
```

Dette er en grunnleggende tilnærming og passer kanskje ikke for komplekse datastrukturer eller spesielle YAML-funksjoner. For sofistikerte behov, må du kanskje se etter eller bidra til en mer omfattende Dart-pakke.
