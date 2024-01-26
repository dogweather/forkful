---
title:                "Refactoring"
date:                  2024-01-26T01:18:47.245158-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Le refactoring est le processus de retravail de votre code pour le rendre plus propre, plus maintenable, sans altérer son comportement externe. Les programmeurs refactorisent pour améliorer la lisibilité, réduire la complexité et rendre la base de code plus susceptible d'être mise à jour ou d'accueillir de nouvelles fonctionnalités.

## Comment faire :
Disons que vous avez un bloc de code où vous effectuez des calculs répétés ou des manipulations de chaînes de caractères à travers plusieurs fonctions. C'est une cible de choix pour le refactoring. Voici un avant-après en utilisant Gleam, qui met fortement l'accent sur la sécurité des types et l'immutabilité :

```gleam
// Avant le refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("La superficie est \(area)")
}

// Après le refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("La superficie est \(area)")
}

// Dans une autre partie de votre code, vous appelerez print_area comme ceci :
print_area(calculate_area(10, 20))
```

Exemple de sortie :
```
La superficie est 200
```

En refactorisant, nous avons rendu `print_area` plus focalisé sur l'impression seule, tandis que le calcul est géré ailleurs, rendant le code plus modulaire et plus facile à réutiliser ou à tester.

## Plongée profonde
Le refactoring, en tant que concept, existe aussi longtemps que la programmation elle-même — revisiter et nettoyer le code fait partie d'une bonne intendance. La formalisation moderne du refactoring, ainsi que de nombreuses techniques et modèles utilisés aujourd'hui, peuvent être retracées jusqu'au livre séminal de Martin Fowler "Refactoring : Améliorer la conception d'un code existant" publié en 1999.

Dans l'écosystème de Gleam, le refactoring a des considérations spécifiques. L'une des plus significatives est la vérification de type forte au moment de la compilation, qui peut aider à attraper les erreurs tôt lorsque vous déplacez les choses. Les fonctionnalités de vérification de motif et d'immutabilité de Gleam peuvent également vous guider pour écrire un code plus clair et plus concis — l'un des objectifs principaux du refactoring.

Les alternatives au refactoring pourraient inclure la réécriture du code à partir de zéro ou la correction du code avec des solutions rapides. Cependant, le refactoring est généralement l'approche la plus sûre et la plus efficace pour améliorer le code existant sans introduire de nouveaux bugs, car il implique des transformations incrémentales, bien soulignées, préservant le comportement.

## Voir aussi
- Le livre de Martin Fowler "Refactoring" : https://martinfowler.com/books/refactoring.html
- Le site web du langage Gleam, avec une documentation et des exemples supplémentaires : https://gleam.run/
- "Refactoring : Améliorer la conception d'un code existant" par Martin Fowler (pour les principes sous-jacents applicables à travers les langues) : https://martinfowler.com/books/refactoring.html