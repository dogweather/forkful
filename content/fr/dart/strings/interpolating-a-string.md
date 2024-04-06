---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:47.532900-07:00
description: "Comment faire : En Dart, l'interpolation de cha\xEEnes est simple, en\
  \ utilisant le symbole `$` pour interpoler directement les expressions au sein des\u2026"
lastmod: '2024-04-05T21:53:58.937316-06:00'
model: gpt-4-0125-preview
summary: "En Dart, l'interpolation de cha\xEEnes est simple, en utilisant le symbole\
  \ `$` pour interpoler directement les expressions au sein des litt\xE9raux de cha\xEE\
  ne ."
title: "Interpolation d'une cha\xEEne de caract\xE8res"
weight: 8
---

## Comment faire :
En Dart, l'interpolation de chaînes est simple, en utilisant le symbole `$` pour interpoler directement les expressions au sein des littéraux de chaîne :

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Interpolation simple de variable
  print('Apprendre $name en $year !');
  // Sortie : Apprendre Dart en 2023 !
  
  // Interpolation d'expressions
  print('Dans deux ans, ce sera ${year + 2}.');
  // Sortie : Dans deux ans, ce sera 2025.
}
```

Dans le cas où vous avez des expressions plus complexes ou souhaitez effectuer des opérations au sein même de la chaîne, encadrez l'expression par `${}`. Dart ne dispose pas de bibliothèques tierces populaires spécifiquement pour l'interpolation de chaînes car il est bien équipé nativement pour gérer des scénarios variés et complexes.
