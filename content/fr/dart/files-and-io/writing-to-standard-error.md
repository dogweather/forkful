---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:08.864518-07:00
description: "Comment faire : En Dart, \xE9crire sur stderr est simple en utilisant\
  \ l'objet `stderr` disponible dans `dart:io`. Voici un exemple de base ."
lastmod: '2024-03-13T22:44:57.408683-06:00'
model: gpt-4-0125-preview
summary: "En Dart, \xE9crire sur stderr est simple en utilisant l'objet `stderr` disponible\
  \ dans `dart:io`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En Dart, écrire sur stderr est simple en utilisant l'objet `stderr` disponible dans `dart:io`. Voici un exemple de base :

```dart
import 'dart:io';

void main() {
  stderr.writeln('Ceci est un message d'erreur.');
}
```

Sortie lors de l'exécution :
```
Ceci est un message d'erreur.
```
Ce message est envoyé au flux stderr, qui est généralement affiché dans la console ou le terminal.

Pour démontrer une complexité plus grande, comme journaliser une exception, l'ensemble riche de fonctionnalités de Dart permet une gestion des erreurs concise et efficace :

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simule une opération pouvant générer une exception
    throw Exception('Quelque chose a mal tourné !');
  } catch (e) {
    stderr.writeln('Erreur : $e');
  }
}

void main() {
  riskyOperation();
}
```

Sortie lors de l'exécution :
```
Erreur : Exception : Quelque chose a mal tourné !
```

Ce modèle est particulièrement utile pour les applications qui ont besoin de séparer les journaux normaux des journaux d'erreur, ce qui facilite le suivi et le débogage des applications.

Bien que la bibliothèque standard de Dart soit assez complète, de nombreux programmes n'exigent pas de bibliothèques tierces pour écrire sur stderr. Cependant, si votre application nécessite des capacités de journalisation plus sophistiquées (par exemple, vers des fichiers, sur le réseau, avec formatage), le package `logging` est un choix populaire. Voici un aperçu rapide de l'utilisation de `logging` pour les erreurs :

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MonJournalApp');

void configurerLaJournalisation() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  configurerLaJournalisation();
  logger.severe('Erreur grave : Quelque chose de très mauvais s'est produit.');
}
```

Sortie lors de l'exécution :
```
SEVERE: 2023-04-01 00:00:00.000: Erreur grave : Quelque chose de très mauvais s'est produit.
```

Cette méthode offre un degré de personnalisation et de contrôle plus élevé sur ce qui est journalisé comme une erreur et comment cela est formaté, ce qui peut être très utile dans des applications plus grandes et plus complexes.
