---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:16.969808-07:00
description: "Les tableaux associatifs en Dart, commun\xE9ment appel\xE9s Maps, sont\
  \ des structures de donn\xE9es qui stockent des donn\xE9es en paires cl\xE9-valeur.\
  \ Ils permettent\u2026"
lastmod: '2024-03-11T00:14:31.397398-06:00'
model: gpt-4-0125-preview
summary: "Les tableaux associatifs en Dart, commun\xE9ment appel\xE9s Maps, sont des\
  \ structures de donn\xE9es qui stockent des donn\xE9es en paires cl\xE9-valeur.\
  \ Ils permettent\u2026"
title: Utilisation des tableaux associatifs
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs en Dart, communément appelés Maps, sont des structures de données qui stockent des données en paires clé-valeur. Ils permettent aux programmeurs d'accéder aux éléments non pas à travers des indices, mais des clés, rendant la récupération de données intuitive et efficace, surtout lorsqu'on travaille avec des données structurées où chaque élément a un identifiant unique.

## Comment faire :

Dart offre une syntaxe simple pour créer et manipuler des Maps. Ci-dessous, des exemples démontrant les opérations de base telles que la création, l'ajout d'éléments et la récupération de valeurs.

```dart
void main() {
  // Création d'une map
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // Ajout d'une nouvelle paire clé-valeur
  fruitColors['orange'] = 'orange';

  // Accès à une valeur par sa clé
  print(fruitColors['apple']); // Sortie : red

  // Mise à jour d'une valeur
  fruitColors['banana'] = 'green';

  // Itération sur la Map
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // Exemple de sortie :
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

Pour des structures de données complexes ou une fonctionnalité étendue, les programmeurs Dart s'appuient souvent sur des bibliothèques supplémentaires. Une de ces bibliothèques est `collection`, qui fournit des types de collection avancés et des utilitaires. Bien que `collection` ne modifie pas la manière fondamentale de gérer les Maps, elle les enrichit avec des fonctions utilitaires et des types de collection plus sophistiqués. Voici comment vous pourriez l'utiliser pour une tâche plus spécifique, comme trier une Map par ses valeurs :

D'abord, assurez-vous que le package `collection` est inclus dans votre fichier `pubspec.yaml` :

```yaml
dependencies:
  collection: ^1.15.0
```

Ensuite, vous pouvez l'utiliser de la manière suivante :

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // Trier la Map par ses valeurs (couleurs)
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // Sortie :
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

Cet exemple démontre le tri des entrées d'une Map basé sur leurs valeurs, montrant comment Dart et son écosystème dynamique peuvent habilement gérer les tableaux associatifs pour une manipulation de données plus sophistiquée.
