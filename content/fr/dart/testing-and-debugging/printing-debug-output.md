---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:52.144816-07:00
description: "Imprimer le d\xE9bogage dans Dart consiste \xE0 afficher des informations\
  \ dans la console pendant l'ex\xE9cution, permettant aux d\xE9veloppeurs de suivre\
  \ le flux\u2026"
lastmod: '2024-03-13T22:44:57.392844-06:00'
model: gpt-4-0125-preview
summary: "Imprimer le d\xE9bogage dans Dart consiste \xE0 afficher des informations\
  \ dans la console pendant l'ex\xE9cution, permettant aux d\xE9veloppeurs de suivre\
  \ le flux\u2026"
title: "Imprimer la sortie de d\xE9bogage"
weight: 33
---

## Quoi et Pourquoi ?

Imprimer le débogage dans Dart consiste à afficher des informations dans la console pendant l'exécution, permettant aux développeurs de suivre le flux d'exécution, d'investiguer l'état des variables ou d'identifier la source des erreurs. Les programmeurs l'utilisent couramment pour le dépannage et vérifier que leur code se comporte comme prévu, facilitant ainsi un processus de développement plus fluide et plus efficace.

## Comment faire :

Dans Dart, vous pouvez imprimer la sortie de débogage en utilisant la fonction `print()`. Voici comment afficher des messages simples et des valeurs de variables :

```dart
void main() {
  String salutation = "Bonjour, Dart !";
  print(salutation); // Imprime : Bonjour, Dart !

  int nombre = 42;
  print('Le nombre est $nombre.'); // Imprime : Le nombre est 42.
}
```

Pour des données structurées, comme des listes ou des objets, la méthode `toString()` de Dart peut ne pas fournir suffisamment de détails. Dans ces cas, vous pouvez utiliser la fonction `jsonEncode` de la bibliothèque `dart:convert` de Dart pour convertir les données en une chaîne JSON plus lisible :

```dart
import 'dart:convert';

void main() {
  var utilisateur = {
    'nom': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(utilisateur));
  // Imprime : {"nom":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

Lorsque des capacités de débogage plus sophistiquées sont nécessaires, comme la journalisation avec différents niveaux d'importance (info, avertissement, erreur), vous pouvez utiliser des bibliothèques tierces comme `logger`. Voici comment l'utiliser :

1. Ajoutez `logger` à votre `pubspec.yaml` :

```yaml
dependencies:
  logger: ^1.0.0
```

2. Utilisez `logger` dans votre code Dart :

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Ceci est un message de débogage");
  logger.w("Ceci est un message d'avertissement");
  logger.e("Ceci est un message d'erreur");
}
```

La sortie sera plus informative, montrant le niveau du message et le message lui-même, ce qui facilite la distinction entre les différents types de messages de journalisation.
