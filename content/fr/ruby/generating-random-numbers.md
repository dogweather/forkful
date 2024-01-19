---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
La génération de nombres aléatoires est un processus par lequel on produit une série de nombres sans aucun motif apparent. Les programmeurs utilisent cela pour entre autres, programmer des simulations, des jeux, et des résultats imprévisibles.

## Comment faire:
Ruby simplifie la génération de nombres aléatoires. Jetez un œil à l'exemple ci-dessous:

```Ruby
rnd = Random.new
puts rnd.rand(1..100)  # Produit un nombre aléatoire entre 1 et 100
```

Résultat possible:

```Ruby
45
```

Vous pouvez aussi avoir un nombre aléatoire entre 0 et 1 uniquement.

```Ruby
puts rnd.rand  # Produit un nombre aléatoire entre 0 et 1
```

Résultat Possible:

```Ruby
0.3423437892749144
```

## Plongée profonde
Historiquement, la génération de nombres aléatoires est un sujet qui a été exploré largement en raison de ses multiples utilisations, des calculs de probabilités à la cryptographie. Ruby utilise une forme de l'algorithme Mersenne Twister pour générer des nombres aléatoires.

Alternativement, en Ruby, on peut aussi utiliser la méthode `Kernel#rand` ou `Array#sample` pour générer des nombres aléatoires. Cependant, `Random#rand` offre plus de flexibilité, permettant de spécifier un range d'où générer le nombre aléatoire.

En termes de détails d'implémentation, chaque instance de `Random` a sa propre graine et séquence de nombres aléatoires, indépendante des autres instances.

## Voir aussi:
Voici quelques liens utiles pour en savoir plus :

- [Documentation officielle de Ruby sur les nombres aléatoires] (https://ruby-doc.org/core-2.7.0/Random.html)
- [Mersenne Twister] (https://fr.wikipedia.org/wiki/Mersenne_twister) 
- [Kernel#rand en Ruby] (https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-rand)
- [Array#sample en Ruby] (https://ruby-doc.org/core-2.7.0/Array.html#method-i-sample)