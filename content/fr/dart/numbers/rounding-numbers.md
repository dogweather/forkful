---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:39.382240-07:00
description: "Arrondir des nombres est le processus qui consiste \xE0 ajuster un nombre\
  \ \xE0 son nombre entier le plus proche ou \xE0 un nombre sp\xE9cifi\xE9 de d\xE9\
  cimales. Les\u2026"
lastmod: '2024-03-11T00:14:31.399671-06:00'
model: gpt-4-0125-preview
summary: "Arrondir des nombres est le processus qui consiste \xE0 ajuster un nombre\
  \ \xE0 son nombre entier le plus proche ou \xE0 un nombre sp\xE9cifi\xE9 de d\xE9\
  cimales. Les\u2026"
title: Arrondissement des nombres
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Arrondir des nombres est le processus qui consiste à ajuster un nombre à son nombre entier le plus proche ou à un nombre spécifié de décimales. Les programmeurs arrondissent souvent les nombres pour simplifier les calculs, améliorer la lisibilité, ou préparer les données pour l'affichage, assurant ainsi la cohérence et la clarté dans les sorties numériques.

## Comment faire :

Dart fournit des méthodes natives dans son type de base `num` pour les opérations d'arrondissement. Ici, nous allons explorer des méthodes comme `round()`, `floor()`, `ceil()`, et comment arrondir à un nombre spécifique de décimales.

### Arrondir au nombre entier le plus proche :

```dart
var number = 3.56;
print(number.round()); // Affiche : 4
```

### Arrondir à l'inférieur :

```dart
print(number.floor()); // Affiche : 3
```

### Arrondir à supérieur :

```dart
print(number.ceil()); // Affiche : 4
```

### Arrondir à un nombre spécifique de décimales :

Pour arrondir à un nombre spécifié de décimales, nous pouvons utiliser la méthode `toStringAsFixed()`, qui retourne une chaîne de caractères, ou utiliser une combinaison de `pow` de `dart:math` pour un résultat numérique.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // À des fins d'affichage
print(roundedString); // Affiche : 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Affiche : 3.57

// Alternativement, pour un résultat numérique :
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Affiche : 3.57
```

Bien que la bibliothèque de base de Dart couvre la plupart des besoins en matière d'arrondissement de manière efficace, pour des opérations mathématiques plus complexes ou des exigences de précision plus élevées, des bibliothèques telles que `decimal` peuvent être utiles. La bibliothèque `decimal` offre un moyen facile de travailler avec des nombres décimaux sans perdre en précision, ce qui est particulièrement pratique pour les calculs financiers, mais pour des méthodes d'arrondissement simples comme celles présentées, la fonctionnalité de base de Dart est généralement suffisante.
