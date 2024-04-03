---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:36.175232-07:00
description: "Kuinka: Dartissa YAML:n k\xE4ytt\xF6 tyypillisesti edellytt\xE4\xE4\
  \ kolmannen osapuolen kirjaston k\xE4ytt\xF6\xE4, koska kieli ei sis\xE4ll\xE4 sis\xE4\
  \xE4nrakennettuja YAML-\u2026"
lastmod: '2024-03-13T22:44:56.293580-06:00'
model: gpt-4-0125-preview
summary: "Dartissa YAML:n k\xE4ytt\xF6 tyypillisesti edellytt\xE4\xE4 kolmannen osapuolen\
  \ kirjaston k\xE4ytt\xF6\xE4, koska kieli ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettuja\
  \ YAML-j\xE4sennysominaisuuksia."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Kuinka:
Dartissa YAML:n käyttö tyypillisesti edellyttää kolmannen osapuolen kirjaston käyttöä, koska kieli ei sisällä sisäänrakennettuja YAML-jäsennysominaisuuksia. Suosittu valinta on `yaml`-paketti. Aloittaaksesi sinun tulee lisätä tämä paketti `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  yaml: ^3.1.0
```

Muista suorittaa `pub get` noutaaksesi paketin.

### YAML:n lukeminen
YAML-tiedoston lukemiseksi, tuo ensin `yaml`-paketti ja käytä sitten `loadYaml`-funktiota:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Tulostus: John Doe
}

```

Olettaen, että `config.yaml`-tiedostosi näyttää tältä:

```yaml
name: John Doe
age: 30
```

### YAML:n kirjoittaminen
Vaikka `yaml`-paketti onkin hieno jäsennykseen, se ei tue YAML:n kirjoittamista. Tätä varten saatat joutua muuntamaan tietosi manuaalisesti YAML:ksi tai käyttämään toista pakettia, jos sellainen on saatavilla. Tai, yksinkertaisemmin, hallitsemaan datamuunnoksiasi ja tulostamaan ne merkkijonoina, jotka vastaavat YAML-syntaksia:

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
  print(toYamlString(data)); // Tulostus: name: Jane Doe
                             //           age: 29
}
```

Tämä on alkeellinen lähestymistapa, eikä se välttämättä sovi monimutkaisiin tietorakenteisiin tai erityisiin YAML-ominaisuuksiin. Monimutkaisempien tarpeiden varalta saatat joutua etsimään tai myötävaikuttamaan kattavampaan Dart-pakettiin.
