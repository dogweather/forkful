---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:10.075877-07:00
description: "Calculer une date dans le futur ou dans le pass\xE9 est une t\xE2che\
  \ courante pour les programmeurs, concernant la planification, les rappels, ou toute\u2026"
lastmod: '2024-03-13T22:44:57.405297-06:00'
model: gpt-4-0125-preview
summary: "Calculer une date dans le futur ou dans le pass\xE9 est une t\xE2che courante\
  \ pour les programmeurs, concernant la planification, les rappels, ou toute\u2026"
title: "Calculer une date dans le futur ou le pass\xE9"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Calculer une date dans le futur ou dans le passé est une tâche courante pour les programmeurs, concernant la planification, les rappels, ou toute fonctionnalité dépendant des calculs de dates. Comprendre comment manipuler les dates est crucial pour les systèmes backend, les interfaces utilisateurs, et l'analyse de données, en particulier pour ceux qui passent à Dart et cherchent à implémenter une logique temporelle de manière efficace.

## Comment faire :
Dart offre un soutien robuste pour la manipulation des dates à travers sa classe `DateTime`. Voici comment vous pouvez calculer des dates futures ou passées en utilisant Dart natif, sans avoir besoin de bibliothèques tierces.

### Calculer une Date Future
Pour calculer une date dans le futur, vous créez un objet `DateTime` et utilisez la méthode `add` avec la durée désirée.

```dart
DateTime aujourd'hui = DateTime.now();
Duration dixJours = Duration(days: 10);
DateTime dateFuture = aujourd'hui.add(dixJours);

print(dateFuture); // Sortie : 2023-04-21 14:22:35.123456 (sortie exemple, dépend de la date et de l'heure actuelles)
```

### Calculer une Date Passée
Pour calculer une date dans le passé, vous utilisez la méthode `subtract` sur un objet `DateTime` avec la durée nécessaire.

```dart
DateTime aujourd'hui = DateTime.now();
Duration ilYaQuinzeJours = Duration(days: 15);
DateTime datePassée = aujourd'hui.subtract(ilYaQuinzeJours);

print(datePassée); // Sortie : 2023-03-27 14:22:35.123456 (sortie exemple, dépend de la date et de l'heure actuelles)
```

### Utiliser des Bibliothèques Tierces
Bien que les capacités natives de Dart pour la manipulation des dates soient puissantes, vous pourriez vous retrouver à avoir besoin d'opérations plus spécifiques, comme analyser ou formater les dates plus facilement, ou effectuer des calculs complexes. Dans de tels cas, le paquet `time` peut être très utile.

D'abord, ajoutez `time` à vos dépendances dans `pubspec.yaml` :

```yaml
dependencies:
  time: ^2.0.0
```

Ensuite, vous pouvez l'utiliser pour effectuer des calculs similaires avec une lisibilité améliorée :

```dart
import 'package:time/time.dart';

void main() {
  DateTime aujourd'hui = DateTime.now();

  // Calculer une date future
  DateTime dateFuture = aujourd'hui + 10.days;
  print(dateFuture); // Format de sortie : 2023-04-21 14:22:35.123456

  // Calculer une date passée
  DateTime datePassée = aujourd'hui - 15.days;
  print(datePassée); // Format de sortie : 2023-03-27 14:22:35.123456
}
```

Ces exemples illustrent les manipulations de base des dates dans Dart, incluant l'ajout et la soustraction de temps à ou d'une date actuelle, démontrant comment les dates peuvent être gérées sans effort dans les applications Dart.
