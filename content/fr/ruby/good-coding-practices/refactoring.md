---
aliases:
- /fr/ruby/refactoring/
date: 2024-01-26 03:36:35.637450-07:00
description: "Le refactorisage est le processus de restructuration du code informatique\
  \ existant sans en changer le comportement externe. Les programmeurs refactorisent\u2026"
lastmod: 2024-02-18 23:09:09.416403
model: gpt-4-0125-preview
summary: "Le refactorisage est le processus de restructuration du code informatique\
  \ existant sans en changer le comportement externe. Les programmeurs refactorisent\u2026"
title: Remaniement de code
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Le refactorisage est le processus de restructuration du code informatique existant sans en changer le comportement externe. Les programmeurs refactorisent pour améliorer les attributs non fonctionnels du logiciel, tels que la lisibilité, la réduction de la complexité, l'amélioration de la maintenabilité ou l'amélioration des performances.

## Comment faire :

Passons à travers un exemple de refactorisage d'une méthode Ruby qui calcule la somme des carrés.

**Avant le refactorisage :**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Sortie : 14
```

**Après le refactorisage :**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Sortie : 14
```

La version refactorisée utilise les Enumerables de Ruby pour exprimer la même logique de manière plus succincte et claire. La méthode `map` transforme chaque élément, et `sum` agrège leurs valeurs, éliminant le besoin de gestion manuelle des boucles et d'assignation de variables.

## Plongeon Profond

Le refactorisage possède un contexte historique riche, remontant aux pratiques initiales dans le développement de logiciels. Les premières mentions peuvent être retracées jusqu'aux années 1990, avec des contributions significatives de Martin Fowler dans son livre "Refactoring: Improving the Design of Existing Code", où il fournit un catalogue de motifs pour le refactorisage. Depuis lors, le refactorisage est devenu une pierre angulaire des pratiques de développement agile.

Lorsque nous parlons d'alternatives au refactorisage, nous devons envisager une approche différente comme la 'Réécriture', où vous remplacez l'ancien système en partie ou en totalité, ou adoptez des pratiques comme les 'Revue de Code' et la 'Programmation en Paire' pour améliorer progressivement la qualité du code. Cependant, ce ne sont pas des remplacements pour le refactorisage ; ils complètent le processus.

En termes de mise en œuvre, Ruby offre une syntaxe excellente et expressive qui résulte souvent en un code plus court et plus lisible après refactorisage. Les principes clés incluent DRY (Don't Repeat Yourself), l'utilisation de noms significatifs, le maintien des méthodes courtes et focalisées sur une seule tâche, et l'utilisation efficace du module Enumerable de Ruby, comme vu dans l'exemple ci-dessus. Des outils automatisés comme RuboCop peuvent également aider les programmeurs à identifier les endroits dans le code qui pourraient bénéficier du refactorisage.

## Voir Aussi

Pour approfondir le refactorisage en Ruby, consultez ces ressources :

- Le livre séminal de Martin Fowler : [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Le guide de style de Ruby pour écrire un code plus propre : [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, un analyseur de code statique (linter) et formateur : [Dépôt GitHub de RuboCop](https://github.com/rubocop/rubocop)
