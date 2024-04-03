---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.056099-07:00
description: "Comment faire : La biblioth\xE8que de base de Dart fournit le package\
  \ `dart:io` pour la gestion des fichiers, vous permettant d'\xE9crire des fichiers\
  \ texte\u2026"
lastmod: '2024-03-13T22:44:57.410882-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que de base de Dart fournit le package `dart:io` pour la\
  \ gestion des fichiers, vous permettant d'\xE9crire des fichiers texte sans n\xE9\
  cessiter de biblioth\xE8ques tierces."
title: "\xC9crire un fichier texte"
weight: 24
---

## Comment faire :
La bibliothèque de base de Dart fournit le package `dart:io` pour la gestion des fichiers, vous permettant d'écrire des fichiers texte sans nécessiter de bibliothèques tierces. Voici un exemple simple d'écriture d'un fichier texte :

```dart
import 'dart:io';

void main() async {
  // Crée un nouveau fichier nommé 'example.txt' dans le répertoire courant.
  var file = File('example.txt');
  
  // Écrit une chaîne de caractères dans le fichier.
  await file.writeAsString('Hello, Dart!');
  
  // Vérifie le contenu.
  print(await file.readAsString()); // Sortie : Hello, Dart!
}
```

Lorsque vous traitez avec des fichiers plus grands ou des flux de données, vous pourriez préférer écrire le contenu en utilisant `openWrite` qui retourne un `IOSink` et vous permet d'écrire des données par blocs :

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Écrit plusieurs lignes dans le fichier.
  sink
    ..writeln('Ligne 1 : Le rapide renard brun saute par-dessus le chien paresseux.')
    ..writeln('Ligne 2 : Dart est génial !')
    ..close();

  // Attendre la fermeture du sink pour s'assurer que toutes les données sont écrites dans le fichier.
  await sink.done;

  // Lire et imprimer le contenu du fichier pour vérifier
  print(await file.readAsString());
}
```

Pour des opérations de fichiers plus avancées, incluant l'ajout à des fichiers ou l'écriture en octets, vous pouvez explorer plus en détail les méthodes de la classe `File` fournies par `dart:io`. De plus, lorsqu'on travaille sur des projets à grande échelle ou plus complexes, la prise en compte de packages comme `path` pour la gestion des chemins de fichiers ou `shelf` pour les fonctionnalités de serveur web pourrait être bénéfique, bien que l'écriture de fichiers directement repose typiquement sur les bibliothèques intégrées de Dart.
