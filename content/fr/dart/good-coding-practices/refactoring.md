---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:01.670315-07:00
description: "Comment faire : Avant le refactoring, vous pourriez avoir un morceau\
  \ de code qui m\xE9lange diff\xE9rents niveaux d'abstraction ou de responsabilit\xE9\
  s, comme\u2026"
lastmod: '2024-04-05T21:53:58.963778-06:00'
model: gpt-4-0125-preview
summary: "Avant le refactoring, vous pourriez avoir un morceau de code qui m\xE9lange\
  \ diff\xE9rents niveaux d'abstraction ou de responsabilit\xE9s, comme calculer une\
  \ remise puis l'appliquer ."
title: Refactorisation
weight: 19
---

## Comment faire :


### Exemple 1 : Renommer et Extraire des Méthodes
Avant le refactoring, vous pourriez avoir un morceau de code qui mélange différents niveaux d'abstraction ou de responsabilités, comme calculer une remise puis l'appliquer :

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Prix final : $finalPrice");
}
```

**Sortie :**
```
Prix final : 80.0
```

Après le refactoring, vous pouvez extraire le calcul de la remise dans sa propre méthode et lui donner un nom significatif :

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Prix final : $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**Sortie :**
```
Prix final : 80.0
```

En extrayant le calcul dans une méthode, vous avez maintenant une opération clairement définie qui peut être réutilisée, testée indépendamment et facilement modifiée.

### Exemple 2 : Simplifier les Expressions Conditionnelles
Avant le refactoring, les déclarations conditionnelles pourraient être trop complexes ou difficiles à lire :

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Remise : $discount");
}
```

**Sortie :**
```
Remise : 0.05
```

Après le refactoring, envisagez d'utiliser une map pour une structure plus claire et des mises à jour ou extensions plus faciles concernant les types de clients et les remises :

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Remise : $discount");
}
```

**Sortie :**
```
Remise : 0.05
```

Ce refactoring rend non seulement le code plus concis mais encapsule également la logique de détermination des remises d'une manière plus facile à comprendre et à maintenir.

### Bibliothèques tierces pour le Refactoring
Quand il s'agit de refactoring dans Dart, spécialement au sein des applications Flutter, la suite [Dart DevTools](https://dart.dev/tools/dart-devtools) est inestimable. Elle inclut des outils de performance, un inspecteur de widget et un débogueur au niveau du code source. Bien que ce ne soit pas une bibliothèque tierce, Dart DevTools est souvent utilisé en parallèle avec des bibliothèques comme `flutter_bloc` pour gérer proprement l'état de manière favorable au refactoring, en vue d'améliorer la modularité et la lisibilité. Malheureusement, en raison de la portée de cette entrée, des exemples de code spécifiques utilisant des bibliothèques tierces ne seront pas fournis ici, mais les développeurs sont encouragés à explorer ces outils pour améliorer le processus de refactoring dans leurs applications Dart/Flutter.
