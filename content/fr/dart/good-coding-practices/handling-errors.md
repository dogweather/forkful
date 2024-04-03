---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:08.798500-07:00
description: "G\xE9rer les erreurs en Dart consiste \xE0 anticiper et g\xE9rer les\
  \ exceptions qui surviennent pendant l'ex\xE9cution du programme pour am\xE9liorer\
  \ la fiabilit\xE9 et\u2026"
lastmod: '2024-03-13T22:44:57.398552-06:00'
model: gpt-4-0125-preview
summary: "G\xE9rer les erreurs en Dart consiste \xE0 anticiper et g\xE9rer les exceptions\
  \ qui surviennent pendant l'ex\xE9cution du programme pour am\xE9liorer la fiabilit\xE9\
  \ et l'utilisabilit\xE9."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
Dart prend en charge deux types d'erreurs : les erreurs *de compilation* et les erreurs *d'exécution*. Les erreurs de compilation sont détectées par l'analyseur Dart avant l'exécution du code, tandis que les erreurs d'exécution, ou exceptions, se produisent pendant l'exécution. Voici comment vous gérez les exceptions en Dart :

### Essayer-Attraper
Utilisez `try-catch` pour capturer les exceptions et les empêcher de faire planter votre application :

```dart
try {
  var result = 100 ~/ 0; // Tentative de division par zéro, lance une exception
} catch (e) {
  print('Exception capturée : $e'); // Gère l'exception
}
```
Sortie d'exemple : `Exception capturée : IntegerDivisionByZeroException`

### Exception Spécifique
Pour gérer des exceptions spécifiques, mentionnez l'exception après `catch` :

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Impossible de diviser par zéro.'); // Gère spécifiquement les exceptions de division par zéro
}
```
Sortie d'exemple : `Impossible de diviser par zéro.`

### Trace de Pile
Pour obtenir une trace de pile pour le débogage, utilisez un second paramètre dans le bloc catch :

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Exception : $e');
  print('Trace de pile : $s'); // Imprime la trace de pile pour le débogage
}
```

### Enfin
Utilisez `finally` pour exécuter du code après try/catch, qu'une exception ait été lancée ou non :

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Exception capturée : $e');
} finally {
  print('Ceci est toujours exécuté.'); // Code de nettoyage ou étapes finales
}
```
Sortie d'exemple :
```
Exception capturée : IntegerDivisionByZeroException
Ceci est toujours exécuté.
```

### Bibliothèques Tiers
Bien que la bibliothèque de base de Dart soit robuste pour le traitement des erreurs, vous pouvez également utiliser des packages tiers comme `dartz` pour la programmation fonctionnelle qui introduit des concepts comme `Either` et `Option` qui peuvent être utilisés pour le traitement des erreurs. Voici un exemple d'utilisation de `dartz` pour le traitement des erreurs :

1. Ajoutez `dartz` à votre fichier `pubspec.yaml` sous dépendances :
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Utilisez `Either` pour gérer les erreurs avec grâce dans votre code Dart :
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividende, int diviseur) {
  if (diviseur == 0) {
    return Left('Impossible de diviser par zéro.');
  } else {
    return Right(dividende ~/ diviseur);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Erreur : $left'), 
    (right) => print('Résultat : $right')
  );
}
```
Sortie d'exemple : `Erreur : Impossible de diviser par zéro.`

La partie `Left` représente généralement l'erreur, et la partie `Right`, le succès. Ce modèle permet de gérer les erreurs de manière plus fonctionnelle, offrant clarté et contrôle sur la gestion des erreurs.
