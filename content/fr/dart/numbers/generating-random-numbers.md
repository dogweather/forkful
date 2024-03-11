---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:44.130436-07:00
description: "La g\xE9n\xE9ration de nombres al\xE9atoires en Dart consiste \xE0 cr\xE9\
  er des valeurs num\xE9riques impr\xE9visibles et diff\xE9rentes \xE0 chaque ex\xE9\
  cution. Les programmeurs\u2026"
lastmod: '2024-03-11T00:14:31.400778-06:00'
model: gpt-4-0125-preview
summary: "La g\xE9n\xE9ration de nombres al\xE9atoires en Dart consiste \xE0 cr\xE9\
  er des valeurs num\xE9riques impr\xE9visibles et diff\xE9rentes \xE0 chaque ex\xE9\
  cution. Les programmeurs\u2026"
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La génération de nombres aléatoires en Dart consiste à créer des valeurs numériques imprévisibles et différentes à chaque exécution. Les programmeurs exploitent cette fonctionnalité pour diverses raisons, allant de la simulation de scénarios réels dans des environnements de test à l'activation de mécanismes de jeu et à l'assurance de la sécurité par le biais de l'aléatoire dans les opérations cryptographiques.

## Comment :

La bibliothèque principale de Dart inclut un support pour la génération de nombres aléatoires avec la classe `Random` trouvée dans `dart:math`. Voici un exemple basique :

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Génère un entier aléatoire entre 0 et 99
  double randomDouble = rand.nextDouble(); // Génère un double aléatoire entre 0.0 et 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Sortie d'échantillon : (Cela variera à chaque exécution)*

```
23
0.6722390975465775
```

Pour les cas d'utilisation nécessitant de l'aléatoire cryptographique, Dart propose le constructeur `Random.secure` :

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Sortie d'échantillon : (Cela variera à chaque exécution)*

```
45
```

Si vous travaillez sur des projets Flutter ou avez besoin de plus de complexité dans l'aléatoire, vous pourriez trouver utile le package `faker` pour générer une large gamme de données aléatoires, telles que des noms, des adresses et des dates.

Pour utiliser `faker`, ajoutez-le d'abord à votre fichier `pubspec.yaml` :

```yaml
dependencies:
  faker: ^2.0.0
```

Puis, importez-le et utilisez-le comme montré :

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Génère un nom aléatoire
  print(faker.address.city()); // Génère un nom de ville aléatoire
}
```

*Sortie d'échantillon :*

```
Josie Runolfsdottir
Est Lysanne
```
