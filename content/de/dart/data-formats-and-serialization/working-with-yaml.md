---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:26.799710-07:00
description: "YAML, die Abk\xFCrzung f\xFCr YAML Ain't Markup Language, ist ein f\xFC\
  r Menschen lesbares Daten-Serialisierungsformat. Programmierer nutzen es f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.603207-06:00'
model: gpt-4-0125-preview
summary: "YAML, die Abk\xFCrzung f\xFCr YAML Ain't Markup Language, ist ein f\xFC\
  r Menschen lesbares Daten-Serialisierungsformat."
title: Arbeiten mit YAML
weight: 41
---

## Was & Warum?

YAML, die Abkürzung für YAML Ain't Markup Language, ist ein für Menschen lesbares Daten-Serialisierungsformat. Programmierer nutzen es für Konfigurationsdateien, Datenaustausch und in Anwendungen, bei denen Daten in einem leicht verständlichen Format gespeichert oder übertragen werden müssen.

## Wie geht das:

Bei Dart umfasst das Arbeiten mit YAML typischerweise die Verwendung einer Drittanbieter-Bibliothek, da die Sprache keine integrierten YAML-Parsing-Fähigkeiten besitzt. Eine beliebte Wahl ist das `yaml`-Paket. Um zu beginnen, müssen Sie dieses Paket zu Ihrem `pubspec.yaml` hinzufügen:

```yaml
dependencies:
  yaml: ^3.1.0
```

Vergessen Sie nicht, `pub get` auszuführen, um das Paket abzurufen.

### YAML lesen

Um eine YAML-Datei zu lesen, importieren Sie zunächst das `yaml`-Paket und verwenden dann die Funktion `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Ausgabe: John Doe
}

```

Angenommen, Ihre `config.yaml`-Datei sieht so aus:

```yaml
name: John Doe
age: 30
```

### YAML schreiben

Obwohl das `yaml`-Paket großartig zum Parsen ist, unterstützt es nicht das Schreiben von YAML. Dafür müssen Sie möglicherweise Ihre Daten manuell in YAML konvertieren oder ein anderes Paket verwenden, falls verfügbar. Oder, direkter, verwalten Sie Ihre Datentransformationen und geben Sie sie als Zeichenfolgen aus, die der YAML-Syntax entsprechen:

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
  print(toYamlString(data)); // Ausgabe: name: Jane Doe
                             //         age: 29
}
```

Dies ist ein grundlegender Ansatz und könnte für komplexe Datenstrukturen oder spezielle YAML-Funktionen nicht geeignet sein. Für anspruchsvollere Bedürfnisse müssen Sie möglicherweise nach einem umfassenderen Dart-Paket suchen oder zu einem solchen beitragen.
