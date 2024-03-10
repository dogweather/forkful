---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.584284-07:00
description: "Un shell interactif (REPL - Read-Evaluate-Print Loop) pour Dart permet\
  \ aux programmeurs de taper et ex\xE9cuter dynamiquement du code Dart ligne par\
  \ ligne\u2026"
lastmod: '2024-03-09T21:06:21.243108-07:00'
model: gpt-4-0125-preview
summary: "Un shell interactif (REPL - Read-Evaluate-Print Loop) pour Dart permet aux\
  \ programmeurs de taper et ex\xE9cuter dynamiquement du code Dart ligne par ligne\u2026"
title: Utiliser un shell interactif (REPL)
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Un shell interactif (REPL - Read-Evaluate-Print Loop) pour Dart permet aux programmeurs de taper et exécuter dynamiquement du code Dart ligne par ligne sans avoir besoin de compiler des scripts entiers. Cet outil est inestimable pour apprendre la syntaxe de Dart, expérimenter avec des extraits de code ou pour déboguer en offrant un retour instantané et en facilitant les tests itératifs.

## Comment :

Dart ne vient pas avec un REPL intégré. Cependant, vous pouvez obtenir une fonctionnalité semblable à REPL en utilisant DartPad (en ligne) ou en utilisant des outils tiers comme `dart_repl`.

**Utiliser DartPad :**

DartPad (https://dartpad.dev) est un éditeur Dart en ligne qui vous permet d'écrire et d'exécuter du code Dart dans votre navigateur web. Bien qu'il ne s'agisse pas d'un REPL traditionnel en ligne de commande, il fournit une expérience similaire pour l'expérimentation rapide.

Il suffit d'aller sur le site, de taper votre code Dart dans le volet de gauche, et de cliquer sur "Exécuter" pour voir le résultat sur la droite.

Exemple :
```dart
void main() {
  print('Bonjour, Dart !');
}
```
Résultat :
```
Bonjour, Dart !
```

**Utiliser `dart_repl` (outil tiers) :**

D'abord, installez `dart_repl` via pub globalement :

```shell
dart pub global activate dart_repl
```

Ensuite, lancez `dart_repl` depuis votre terminal :

```shell
dart_repl
```

Maintenant, vous pouvez commencer à taper des instructions Dart directement dans le shell. Par exemple :

```dart
>>> print('Bonjour, REPL !');
Bonjour, REPL !
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Ces méthodes fournissent un chemin rapide pour essayer du code Dart à la volée, facilitant grandement la courbe d'apprentissage et améliorant la productivité.
