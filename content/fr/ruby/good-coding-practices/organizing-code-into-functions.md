---
date: 2024-01-26 01:11:29.912702-07:00
description: "Comment faire : Imaginez que vous \xE9crivez un script rapide pour saluer\
  \ les utilisateurs ."
lastmod: '2024-04-05T21:53:59.828284-06:00'
model: gpt-4-1106-preview
summary: "Imaginez que vous \xE9crivez un script rapide pour saluer les utilisateurs\
  \ ."
title: Organisation du code en fonctions
weight: 18
---

## Comment faire :
Imaginez que vous écrivez un script rapide pour saluer les utilisateurs :

```Ruby
def greet(name)
  "Bonjour, #{name} !"
end

puts greet("Alice")   # Sortie : Bonjour, Alice !
puts greet("Bob")     # Sortie : Bonjour, Bob !
```

Ou peut-être calculez-vous l'aire d'un cercle :

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Sortie : 78.53981633974483
```

Plus propre et plus facile à gérer, n'est-ce pas ?

## Plongée en profondeur
Le concept de fonctions, également connu sous le nom de méthodes en Ruby, n'est pas nouveau - il est aussi vieux que la programmation elle-même. Revenant aux années 1950, les sous-routines, comme elles étaient appelées, ont été introduites pour réduire la redondance.

Des alternatives ? Bien sûr, vous avez le code en ligne, vous pourriez opter pour la POO avec des classes et des objets, ou même functional avec des lambdas et des procs. Mais les fonctions sont le pilier d'un code ordonné. Vous voulez de la performance ? Les variables locales dans les fonctions sont rapides et les fonctions peuvent renvoyer des valeurs immédiatement avec `return`.

En termes de mise en œuvre, vous pouvez définir une fonction avec `def` et la terminer avec `end`. Vous pouvez définir des paramètres par défaut, utiliser des opérateurs splat pour des fonctions variadiques, et plus encore. Les fonctions peuvent être aussi simples ou complexes que vous le voulez.

## Voir aussi
- [Documentation sur les méthodes de Ruby](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program par Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby par Sandi Metz](https://www.poodr.com/)
