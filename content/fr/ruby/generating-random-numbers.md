---
title:                "Génération de nombres aléatoires"
html_title:           "Ruby: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Générer des nombres aléatoires est un moyen pour les programmeurs de créer des valeurs aléatoires dans leurs programmes. Cela peut servir à de nombreuses fins, telles que des jeux, des études de données et des tests d'algorithmes.

# Comment faire:

```Ruby
# Générer un nombre aléatoire entre 1 et 10
rand(1..10)
# => 7

# Générer un nombre aléatoire entre 0 et 1
rand
# => 0.23175719101114115
```

# Plongée en profondeur:

Il existe différentes techniques pour générer des nombres aléatoires, allant de l'utilisation de générateurs pseudo-aléatoires à base de données aléatoires matérielles. Certaines fois, il est possible d'ajuster la "graine" ou la "seed" utilisée pour générer les nombres aléatoires afin d'obtenir une séquence spécifique de valeurs aléatoires.

Il existe également des alternatives à la fonction rand() de Ruby, telle que la méthode Kernel.srand(), qui permet de définir la graine et d'utiliser des algorithmes plus complexes pour générer des nombres aléatoires.

# Voir aussi:

- [La documentation officielle de Ruby sur la fonction rand()](https://ruby-doc.org/core-2.7.1/Random.html#method-c-rand)
- [Un article sur différents types de générateurs aléatoires](https://www.geeksforgeeks.org/random-vs-pseudorandom-numbers/)
- [Quelques outils Ruby pour la génération de nombres aléatoires](https://rubygems.org/search?utf8=%E2%9C%93&query=random)