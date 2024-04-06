---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:39.954536-07:00
description: "Comment faire : Dart fournit des m\xE9thodes simples et directes pour\
  \ la manipulation des cha\xEEnes de caract\xE8res. Pour capitaliser un mot ou une\
  \ phrase, vous\u2026"
lastmod: '2024-03-13T22:44:57.355028-06:00'
model: gpt-4-0125-preview
summary: "Dart fournit des m\xE9thodes simples et directes pour la manipulation des\
  \ cha\xEEnes de caract\xE8res."
title: "Mettre une cha\xEEne de caract\xE8res en majuscules"
weight: 2
---

## Comment faire :


### En utilisant les méthodes intégrées de Dart
Dart fournit des méthodes simples et directes pour la manipulation des chaînes de caractères. Pour capitaliser un mot ou une phrase, vous prendriez typiquement le premier caractère, le convertiriez en majuscule, puis le concaténeriez avec le reste de la chaîne. Voici comment vous pourriez l'implémenter :

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // Sortie : Hello world
}
```

### Capitalisant chaque mot
Pour capitaliser la première lettre de chaque mot dans une chaîne, vous pourriez diviser la chaîne en mots, capitaliser chacun d'eux, puis les rejoindre ensemble :

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // Sortie : Hello Dart Enthusiasts
}
```

### Utilisant des bibliothèques tierces
Bien que la bibliothèque standard de Dart couvre les besoins de base, certaines tâches pourraient être plus facilement accomplies en utilisant des packages tiers. Un choix populaire pour des capacités de manipulation des chaînes étendues, y compris la capitalisation, est le package [`recase`](https://pub.dev/packages/recase). Après l'avoir ajouté au `pubspec.yaml` de votre projet, vous pouvez facilement capitaliser des chaînes parmi d'autres fonctionnalités :

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // Sortie : Hello World
}
```

En utilisant `recase`, vous pouvez capitaliser des mots individuels, des phrases entières, ou même suivre d'autres conventions de casse sans manipuler manuellement les transformations de chaîne.
