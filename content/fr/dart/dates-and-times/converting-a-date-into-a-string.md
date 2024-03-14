---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:32.350009-07:00
description: "Convertir une date en cha\xEEne de caract\xE8res dans Dart est une t\xE2\
  che courante lorsque vous avez besoin d'afficher des informations de date et d'heure\
  \ dans\u2026"
lastmod: '2024-03-13T22:44:57.403108-06:00'
model: gpt-4-0125-preview
summary: "Convertir une date en cha\xEEne de caract\xE8res dans Dart est une t\xE2\
  che courante lorsque vous avez besoin d'afficher des informations de date et d'heure\
  \ dans\u2026"
title: "Convertir une date en cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Convertir une date en chaîne de caractères dans Dart est une tâche courante lorsque vous avez besoin d'afficher des informations de date et d'heure dans un format lisible par l'homme, ou lorsque vous avez l'intention de sérialiser des données pour le stockage ou la transmission. Ce processus permet une représentation et une manipulation faciles des valeurs de date-heure dans un format à la fois compréhensible et personnalisable en fonction du cas d'utilisation.

## Comment faire :

Dart fournit la classe `DateTime` pour la gestion des dates et des heures, et le package `intl` pour le formatage. Premièrement, assurez-vous d'avoir le package `intl` en ajoutant `intl: ^0.17.0` (ou la version la plus récente) à votre fichier `pubspec.yaml`.

### Utilisation de la bibliothèque de base de Dart

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Sortie : 2023-4-12 (par exemple, cela dépend de la date actuelle)
```

Cet exemple construit directement une chaîne à partir des propriétés de `DateTime`.

### Utilisation du package `intl`

Premièrement, importez le package :

```dart
import 'package:intl/intl.dart';
```

Ensuite, formatez la date :

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Sortie : 2023-04-12
```

Le package `intl` permet un formatage beaucoup plus complexe facilement, y compris des formats spécifiques à la locale :

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Sortie : 12 avril 2023
```

Ces exemples montrent des moyens simples mais puissants de convertir et de formater des dates en chaînes de caractères dans Dart, soit en utilisant les fonctionnalités de base de Dart, soit en utilisant le package `intl` pour des options de formatage plus avancées.
