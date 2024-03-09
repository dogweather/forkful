---
title:                "Travailler avec des nombres complexes"
date:                  2024-03-08T21:57:41.645879-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les nombres complexes, constitués d'une partie réelle et d'une partie imaginaire (généralement notés sous la forme a + bi), étendent le concept des nombres sans dimension à un espace bidimensionnel. Les programmeurs travaillent avec des nombres complexes dans des domaines tels que le génie électrique, l'informatique quantique et la dynamique des fluides pour modéliser des phénomènes qui ne peuvent pas être représentés le long d'une seule dimension de nombres réels.

## Comment faire :

Dart lui-même n'inclut pas de bibliothèque intégrée pour les nombres complexes, nécessitant soit la mise en œuvre d'une classe de nombre complexe personnalisée, soit l'utilisation d'une bibliothèque tierce. Un choix populaire pour les tâches de calcul scientifique, qui inclut le support pour les nombres complexes, est `package:scidart`.

### Implémenter une classe de nombre complexe basique

Pour des opérations simples, vous pouvez facilement définir votre propre classe de nombre complexe :

```dart
class Complexe {
  final double reel;
  final double imaginaire;

  Complexe(this.reel, this.imaginaire);

  // Addition de deux nombres complexes
  Complexe operator +(Complexe autre) {
    return Complexe(reel + autre.reel, imaginaire + autre.imaginaire);
  }

  // Représentation en chaîne pour faciliter le débogage
  @override
  String toString() => '${reel} + ${imaginaire}i';
}

void main() {
  var nombre1 = Complexe(3, 4);
  var nombre2 = Complexe(1, 2);

  var somme = nombre1 + nombre2;
  print(somme);  // 4.0 + 6.0i
}
```

### Utiliser SciDart pour des opérations avancées

Pour des opérations plus complexes ou lorsque les performances sont critiques, le `package:scidart` offre un support complet pour les nombres complexes parmi d'autres fonctionnalités de calcul scientifique. Tout d'abord, ajoutez SciDart à votre fichier pubspec.yaml :

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Voici comment réaliser des opérations de base avec des nombres complexes en utilisant SciDart :

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Création de nombres complexes
  var complexeNum1 = Complexe(reel: 5, imaginaire: 3);
  var complexeNum2 = Complexe(reel: 2, imaginaire: 7);

  // Addition
  var somme = complexAdd(complexeNum1, complexeNum2);
  
  // Multiplication
  var produit = complexMultiply(complexeNum1, complexeNum2);

  print('Somme: ${somme.toString()}');  // Somme: Complexe(reel: 7.0, imaginaire: 10.0)
  print('Produit: ${produit.toString()}');  // Produit: Complexe(reel: -11.0, imaginaire: 41.0)
}
```

Ces exemples démontrent la manipulation et l'utilisation basiques des nombres complexes en Dart, à la fois par l'implémentation personnalisée et via la bibliothèque SciDart, soulignant la flexibilité et la puissance de Dart pour les tâches de calcul scientifique.
