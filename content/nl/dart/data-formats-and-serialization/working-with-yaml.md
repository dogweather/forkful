---
title:                "Werken met YAML"
date:                  2024-03-08T21:57:42.221311-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, een afkorting voor YAML Ain't Markup Language, is een voor mensen leesbaar gegevensserialisatieformaat. Programmeurs gebruiken het voor configuratiebestanden, gegevensuitwisseling en in toepassingen waar gegevens moeten worden opgeslagen of verzonden in een formaat dat gemakkelijk te begrijpen is.

## Hoe:

In Dart houdt werken met YAML meestal in dat je een externe bibliotheek gebruikt, omdat de taal geen ingebouwde YAML-parsermogelijkheden bevat. Een populaire keuze is het `yaml` pakket. Om te beginnen moet je dit pakket toevoegen aan je `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Vergeet niet om `pub get` uit te voeren om het pakket op te halen.

### YAML Lezen

Om een YAML-bestand te lezen, importeer je eerst het `yaml` pakket en gebruik je vervolgens de `loadYaml` functie:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Uitvoer: John Doe
}
```

Uitgaande van dat je `config.yaml` bestand er zo uitziet:

```yaml
name: John Doe
age: 30
```

### YAML Schrijven

Hoewel het `yaml` pakket geweldig is voor parsing, ondersteunt het geen schrijven van YAML. Hiervoor moet je misschien je gegevens handmatig naar YAML converteren of een ander pakket gebruiken als dat beschikbaar is. Of, eenvoudiger, beheer je gegevenstransformaties en geef ze uit als strings die overeenkomen met YAML-syntax:

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
  print(toYamlString(data)); // Uitvoer: name: Jane Doe
                             //          age: 29
}
```

Dit is een basale aanpak en is misschien niet geschikt voor complexe gegevensstructuren of speciale YAML-functies. Voor geavanceerde behoeften moet je mogelijk zoeken naar of bijdragen aan een uitgebreider Dart-pakket.
