---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:23.178844-07:00
description: "Comment faire : En Dart, vous pouvez convertir une cha\xEEne en minuscules\
  \ en utilisant la m\xE9thode `toLowerCase()` fournie par la classe `String`. Cette\u2026"
lastmod: '2024-03-13T22:44:57.359669-06:00'
model: gpt-4-0125-preview
summary: "En Dart, vous pouvez convertir une cha\xEEne en minuscules en utilisant\
  \ la m\xE9thode `toLowerCase()` fournie par la classe `String`."
title: "Convertir une cha\xEEne en minuscules"
weight: 4
---

## Comment faire :
En Dart, vous pouvez convertir une chaîne en minuscules en utilisant la méthode `toLowerCase()` fournie par la classe `String`. Cette méthode retourne une nouvelle chaîne avec tous les caractères en majuscules convertis en minuscules. Voyons comment cela fonctionne avec un exemple simple :

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Sortie : hello, world!
}
```

Dart ne nécessite pas de bibliothèques externes pour les tâches basiques de manipulation des chaînes de caractères, y compris la conversion en minuscules, car la classe `String` de la bibliothèque standard est assez complète. Cependant, pour des manipulations plus complexes impliquant des règles spécifiques à la locale, vous pourriez envisager le package `intl`, qui fournit des fonctions d'internationalisation et de localisation, y compris la conversion de casse en fonction de la locale :

Pour utiliser `intl`, ajoutez-le à votre fichier `pubspec.yaml` :

```yaml
dependencies:
  intl: ^0.17.0
```

Ensuite, vous pouvez utiliser la méthode `toLocaleLowerCase()` pour convertir une chaîne en minuscules en fonction de locales spécifiques :

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Locale turque
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Sortie : istanbul
  
  // Locale par défaut (en)
  print(originalString.toLowerCase()); // Sortie : i̇stanbul
}
```

Dans cet exemple, remarquez comment la locale turque gère correctement le 'i' sans point, mettant en évidence l'importance des transformations sensibles à la locale dans les applications internationalisées.
