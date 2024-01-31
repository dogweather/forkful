---
title:                "Refactoring"
date:                  2024-01-26T01:17:30.135843-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

category:             "Elixir"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Le refactoring est le processus de restructuration du code existant sans en changer le comportement externe, visant à améliorer les attributs non fonctionnels tels que la lisibilité et la maintenabilité. Les programmeurs le font pour rendre le code plus propre, plus facile à comprendre et plus efficace, facilitant les mises à jour futures et réduisant le risque de bogues.

## Comment faire :
Nettoyons un motif commun en Elixir. Nous allons refactoriser une fonction `calculate_stats` qui fait plus que ce qu'elle devrait en la divisant en petits morceaux réutilisables.

```elixir
defmodule Stats do
  # Code original, non refactorisé
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # Code refactorisé
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# Exemple de sortie
# Avant le Refactoring
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# Après le Refactoring
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
Comme vous pouvez le voir, la sortie reste la même, mais maintenant nous avons des fonctions modulaires qui peuvent être réutilisées et testées indépendamment.

## Plongée profonde
Le refactoring n'est pas un concept nouveau ; il a été une partie cruciale de la programmation depuis les premiers jours du développement logiciel. Des œuvres notables, telles que "Refactoring: Improving the Design of Existing Code" de Martin Fowler, fournissent des pratiques fondamentales pour le refactoring avec des aperçus de quand et comment les appliquer.

Les alternatives au refactoring manuel incluent les outils d'analyse de code automatisés, qui peuvent suggérer ou même effectuer des refactorisations. Cependant, les outils automatisés peuvent ne pas toujours saisir le contexte complet du code et peuvent manquer des subtilités qu'un examinateur humain attraperait.

Les détails d'implémentation spécifiques à Elixir comprennent la compréhension du paradigme fonctionnel et l'utilisation du matching de motifs, des clauses de garde et de l'opérateur pipe pour écrire un code clair et concis. Par exemple, le refactoring implique souvent la conversion de fonctions complexes de style impératif en fonctions plus petites et composable qui suivent la préférence d'Elixir pour l'immutabilité et les opérations sans effets secondaires.

## Voir également
Pour en savoir plus sur les techniques de refactoring spécifiques à Elixir :

- [Les guides officiels d'Elixir](https://elixir-lang.org/getting-started/)
- ["Refactoring: Improving the Design of Existing Code" de Martin Fowler](https://martinfowler.com/books/refactoring.html), pour des principes généraux qui peuvent être appliqués à Elixir.
- [Credo, un outil d'analyse de code statique pour Elixir](https://github.com/rrrene/credo) qui encourage les meilleures pratiques.
- [Exercism Elixir Track](https://exercism.org/tracks/elixir), pour des exercices pratiques qui impliquent souvent du refactoring.
