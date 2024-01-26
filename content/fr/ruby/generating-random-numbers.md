---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:50:00.649111-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
La génération de nombres aléatoires, c'est produire une séquence de nombres qui n'ont pas de motif prévisible. Les programmeurs en ont besoin pour tout, depuis la sécurité informatique jusqu'aux jeux et simulations.

## How to:
```Ruby
# Générer un nombre aléatoire entre 0 et 1
random_float = rand
puts random_float # => ex: 0.43702236510325

# Générer un nombre aléatoire entre 0 et 99
random_int = rand(100)
puts random_int # => ex: 42

# Générer un nombre dans une plage de valeurs (10 à 20 inclus)
random_range = rand(10..20)
puts random_range # => ex: 15
```

## Deep Dive
Avant le Ruby moderne, les gens utilisaient des méthodes plus primitives pour obtenir des nombres aléatoires, comme le jet de dés ou la rotation de roulettes. Désormais, Ruby utilise des algorithmes complexes pour simuler l'aléatoire - bien sûr ce n’est jamais totalement aléatoire, c'est ce qu'on appelle du pseudo-aléatoire. Un point intéressant: `rand` utilise une graine (ou seed) pour initier sa séquence. Si vous voulez reproduire une séquence précise, vous pouvez initialiser cette graine avec `srand`. Alternativement, Ruby offre aussi la classe `Random` pour plus de contrôle et de fonctionnalités.

## See Also
- [Random numbers in Ruby](https://ruby-doc.org/core-2.7.0/Random.html) : documentation officielle de Ruby sur les nombres aléatoires.
- [Secure Random](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html) : pour quand vous avez besoin de nombres aléatoires pour la cryptographie.
- [Stack Overflow: "How to generate a random number in Ruby"](https://stackoverflow.com/questions/198460/how-to-get-a-random-number-in-ruby) : une discussion sur la génération de nombres aléatoires en Ruby.
