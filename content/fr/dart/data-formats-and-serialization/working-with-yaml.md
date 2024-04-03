---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:30.339218-07:00
description: "Comment faire : Dans Dart, travailler avec YAML implique g\xE9n\xE9\
  ralement l'utilisation d'une biblioth\xE8que tierce car le langage ne comprend pas\
  \ de capacit\xE9s\u2026"
lastmod: '2024-03-13T22:44:57.413457-06:00'
model: gpt-4-0125-preview
summary: "Dans Dart, travailler avec YAML implique g\xE9n\xE9ralement l'utilisation\
  \ d'une biblioth\xE8que tierce car le langage ne comprend pas de capacit\xE9s d'analyse\
  \ YAML int\xE9gr\xE9es."
title: Travailler avec YAML
weight: 41
---

## Comment faire :
Dans Dart, travailler avec YAML implique généralement l'utilisation d'une bibliothèque tierce car le langage ne comprend pas de capacités d'analyse YAML intégrées. Un choix populaire est le paquet `yaml`. Pour commencer, vous devrez ajouter ce paquet à votre `pubspec.yaml` :

```yaml
dependencies:
  yaml: ^3.1.0
```

N'oubliez pas d'exécuter `pub get` pour récupérer le paquet.

### Lire le YAML
Pour lire un fichier YAML, commencez par importer le paquet `yaml`, puis utilisez la fonction `loadYaml` :

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Sortie : John Doe
}

```

En supposant que votre fichier `config.yaml` ressemble à cela :

```yaml
name: John Doe
age: 30
```

### Écrire en YAML
Bien que le paquet `yaml` soit excellent pour l'analyse, il ne prend pas en charge l'écriture en YAML. Pour cela, vous devrez peut-être convertir vos données en YAML manuellement ou utiliser un autre paquet si disponible. Ou, de manière plus simple, gérer vos transformations de données et les sortir comme des chaînes correspondant à la syntaxe YAML :

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
  print(toYamlString(data)); // Sortie : name: Jane Doe
                             //         age: 29
}
```

Cette méthode est rudimentaire et peut ne pas convenir aux structures de données complexes ou aux fonctionnalités spéciales de YAML. Pour des besoins plus sophistiqués, vous pourriez devoir rechercher ou contribuer à un paquet Dart plus complet.
