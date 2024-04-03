---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:37.563231-07:00
description: "Obtenir la date actuelle en Dart implique de demander au syst\xE8me\
  \ la date et l'heure actuelles. Cette fonctionnalit\xE9 est couramment utilis\xE9\
  e dans les\u2026"
lastmod: '2024-03-13T22:44:57.401952-06:00'
model: gpt-4-0125-preview
summary: "Obtenir la date actuelle en Dart implique de demander au syst\xE8me la date\
  \ et l'heure actuelles."
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
La bibliothèque de base de Dart fournit un accès simple à la date et à l'heure actuelles grâce à la classe `DateTime`. Voici l'exemple de base pour obtenir la date actuelle :

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Exemple de sortie : 2023-04-12 10:00:00.000
}
```

Si vous avez besoin uniquement de la partie date (année, mois, jour), vous pouvez formater l'objet `DateTime` :

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Exemple de sortie : 2023-04-12
}
```

Dart n'inclut pas de bibliothèque intégrée pour un formatage de date plus complexe, mais vous pouvez utiliser le package `intl` à cette fin. Tout d'abord, ajoutez le package à votre `pubspec.yaml` :

```yaml
dependencies:
  intl: ^0.17.0
```

Ensuite, vous pouvez facilement formater les dates :

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Exemple de sortie : 2023-04-12
}
```

Pour plus d'options de formatage avancées, explorez la classe `DateFormat` fournie par le package `intl`, qui prend en charge une large gamme de motifs et de paramètres régionaux.
