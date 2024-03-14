---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.487502-07:00
description: "Chercher et remplacer du texte en Dart consiste \xE0 examiner des cha\xEE\
  nes de caract\xE8res pour trouver certains motifs ou s\xE9quences de caract\xE8\
  res et les\u2026"
lastmod: '2024-03-13T22:44:57.357324-06:00'
model: gpt-4-0125-preview
summary: "Chercher et remplacer du texte en Dart consiste \xE0 examiner des cha\xEE\
  nes de caract\xE8res pour trouver certains motifs ou s\xE9quences de caract\xE8\
  res et les\u2026"
title: Recherche et remplacement de texte
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Chercher et remplacer du texte en Dart consiste à examiner des chaînes de caractères pour trouver certains motifs ou séquences de caractères et les substituer par un nouveau contenu. Cette opération est fondamentale pour des tâches telles que la validation de données, la mise en forme de la sortie, l'analyse de l'entrée utilisateur, ou même la manipulation d'URLs et de chemins de fichier, rendant les applications plus dynamiques et réactives aux besoins des utilisateurs.

## Comment faire :

Dart fournit des méthodes robustes pour chercher et remplacer du texte directement à travers sa classe `String`, sans nécessiter de bibliothèques externes. Voici comment vous pouvez le faire :

### Recherche et remplacement basiques

Pour rechercher une sous-chaîne et la remplacer par une autre chaîne, vous pouvez utiliser `replaceAll` :

```dart
String sampleText = "Hello, Dart! Dart is great.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Sortie : Hello, Flutter! Flutter is great.
```

### Utilisation des expressions régulières

Pour des besoins de recherche et de remplacement plus complexes, Dart utilise des expressions régulières via la classe `RegExp`. Cela permet de faire correspondre des motifs et de remplacer des chaînes :

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Sortie : Dart 2024, Flutter 2024
```

Cet exemple trouve toutes les instances d'un ou plusieurs chiffres (`\d+`) dans la chaîne et les remplace par "2024".

### Recherche insensible à la casse

Pour effectuer une recherche insensible à la casse, vous pouvez modifier le constructeur `RegExp` pour ignorer la casse :

```dart
String sampleText = "Welcome to Dart, the programming language.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Sortie : Welcome to Flutter, the programming language.
```

### Remplacement avec une fonction

Pour des remplacements dynamiques basés sur la correspondance elle-même, Dart permet de passer une fonction à `replaceAllMapped`. Cette fonction peut effectuer des opérations ou des calculs sur les séquences correspondantes :

```dart
String sampleText = "Increment 5 by 1 to get 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Sortie : Increment 6 by 1 to get 7.
```

Ceci remplace chaque séquence de chiffres par sa valeur incrémentée. Chaque correspondance est analysée en un entier, incrémentée, puis convertie à nouveau en une chaîne de caractères pour le remplacement.

Les capacités de manipulation de chaînes de caractères de Dart, en particulier pour la recherche et le remplacement de texte, en font un outil puissant pour traiter et préparer les données au sein de vos applications. Que vous utilisiez des remplacements de chaînes simples ou que vous exploitiez la puissance des expressions régulières, Dart offre la flexibilité et la performance nécessaires pour une manipulation efficace du texte.
