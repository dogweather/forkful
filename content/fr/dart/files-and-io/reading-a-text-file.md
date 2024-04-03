---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:31.931752-07:00
description: "Comment faire : La biblioth\xE8que principale de Dart, `dart:io`, fournit\
  \ les fonctionnalit\xE9s n\xE9cessaires pour lire des fichiers texte de mani\xE8\
  re synchrone\u2026"
lastmod: '2024-03-13T22:44:57.409802-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que principale de Dart, `dart:io`, fournit les fonctionnalit\xE9\
  s n\xE9cessaires pour lire des fichiers texte de mani\xE8re synchrone ou asynchrone."
title: Lecture d'un fichier texte
weight: 22
---

## Comment faire :
La bibliothèque principale de Dart, `dart:io`, fournit les fonctionnalités nécessaires pour lire des fichiers texte de manière synchrone ou asynchrone. Voici comment aborder les deux.

**De manière synchrone :**

```dart
import 'dart:io';

void main() {
  var fileName = "chemin/vers/votre/fichier.txt";
  var file = File(fileName);

  // Lecture du fichier de manière synchrone
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Erreur de lecture du fichier : $e');
  }
}
```

**De manière asynchrone :**

Pour éviter de bloquer le programme pendant la lecture du fichier, particulièrement utile pour les gros fichiers ou les applications réactives :

```dart
import 'dart:io';

void main() async {
  var fileName = "chemin/vers/votre/fichier.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Erreur de lecture du fichier : $e');
  }
}
```

**Exemple de sortie :**

Si votre fichier texte contient :

```
Bonjour, Dart !
```

Les deux méthodes ci-dessus produiront :

```
Bonjour, Dart !
```

**Utiliser une bibliothèque tierce :**

Pour des fonctionnalités supplémentaires comme des opérations sur fichiers simplifiées ou une gestion des erreurs améliorée, vous pourriez envisager des bibliothèques tierces telles que `package:file`. Cependant, selon ma dernière mise à jour, utiliser directement le paquet `dart:io`, comme montré ci-dessus, est la méthode la plus courante et la plus simple pour lire des fichiers texte en Dart.
