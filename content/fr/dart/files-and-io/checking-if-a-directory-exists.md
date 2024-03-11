---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:29.590639-07:00
description: "V\xE9rifier si un r\xE9pertoire existe en Dart consiste \xE0 v\xE9rifier\
  \ la pr\xE9sence d'un r\xE9pertoire \xE0 un chemin sp\xE9cifi\xE9 sur le syst\xE8\
  me de fichiers avant d'effectuer\u2026"
lastmod: '2024-03-11T00:14:31.422289-06:00'
model: gpt-4-0125-preview
summary: "V\xE9rifier si un r\xE9pertoire existe en Dart consiste \xE0 v\xE9rifier\
  \ la pr\xE9sence d'un r\xE9pertoire \xE0 un chemin sp\xE9cifi\xE9 sur le syst\xE8\
  me de fichiers avant d'effectuer\u2026"
title: "V\xE9rification de l'existence d'un r\xE9pertoire"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Vérifier si un répertoire existe en Dart consiste à vérifier la présence d'un répertoire à un chemin spécifié sur le système de fichiers avant d'effectuer des opérations comme lire ou écrire des fichiers. Les programmeurs font cela pour éviter les erreurs qui surviennent lorsqu'ils tentent d'accéder ou de modifier des répertoires qui n'existent pas.

## Comment faire :

Dart utilise la bibliothèque `dart:io` pour travailler avec les fichiers et les répertoires. Voici une manière simple de vérifier si un répertoire existe :

```dart
import 'dart:io';

void main() {
  var directory = Directory('chemin/vers/votre/répertoire');

  if (directory.existsSync()) {
    print('Le répertoire existe');
  } else {
    print('Le répertoire n\'existe pas');
  }
}
```
Exemple de sortie si le répertoire existe :
```
Le répertoire existe
```

Ou, s'il n'existe pas :
```
Le répertoire n'existe pas
```

Pour gérer des scénarios plus complexes, tels que la vérification asynchrone ou la création d'un répertoire s'il n'existe pas, vous pourriez utiliser l'approche suivante :

```dart
import 'dart:io';

void main() async {
  var directory = Directory('chemin/vers/votre/répertoire');

  // Vérifier de manière asynchrone si le répertoire existe
  var exists = await directory.exists();
  if (exists) {
    print('Le répertoire existe');
  } else {
    print('Le répertoire n\'existe pas, création en cours...');
    await directory.create(); // Cela crée le répertoire
    print('Répertoire créé');
  }
}
```

Exemple de sortie si le répertoire n'existait pas et a été créé :
```
Le répertoire n'existe pas, création en cours...
Répertoire créé
```

Les capacités intégrées à Dart sont généralement suffisantes pour gérer les fichiers et les répertoires, donc les bibliothèques tierces ne sont généralement pas nécessaires pour cette tâche. Cependant, pour des opérations plus complexes sur le système de fichiers, des paquets comme `path` (pour manipuler les chemins d'une manière indépendante de la plateforme) peuvent compléter la bibliothèque `dart:io` mais n'offrent pas directement de vérifications de l'existence de répertoires plus avancées que ce qui est montré.
