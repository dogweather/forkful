---
title:    "Elixir: Production de nombres aléatoires"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

#

## Pourquoi
La génération de nombres aléatoires est un élément essentiel dans de nombreux programmes de développement. Cela permet aux développeurs de créer de la variété et de créer des simulations réalistes pour tester leurs applications.

## Comment faire
La génération de nombres aléatoires en Elixir peut être facilement réalisée en utilisant la fonction `:rand.uniform/1`. Cette fonction prend un argument entier et renvoie un nombre aléatoire entier compris entre 0 et cet argument (non inclus). Voyons un exemple:

```Elixir
random_number = :rand.uniform(10)
IO.puts("Le nombre aléatoire est #{random_number}")
```

Cet exemple affichera un nombre aléatoire compris entre 0 et 9. Vous pouvez également utiliser cette fonction pour générer des nombres aléatoires à virgule flottante en utilisant `:rand.uniform/0`, qui renvoie un nombre entre 0.0 et 1.0.

Elixir propose également une fonction `:rand.seed/1` qui permet de choisir une graine (seed) pour générer des nombres aléatoires. Cela peut être utile pour reproduire les mêmes séquences de nombres aléatoires lors de tests ou de simulations. Voyons un exemple:

```Elixir
:rand.seed(42)
IO.inspect(:rand.uniform(100))
IO.inspect(:rand.uniform(100))
```

Cet exemple renverra toujours les mêmes deux nombres aléatoires, quelle que soit la manière dont il est exécuté.

## Approfondissement
Il est important de noter que la génération de nombres aléatoires en informatique n'est pas réellement aléatoire, mais plutôt pseudo-aléatoire. Cela signifie que les nombres générés suivent une séquence déterministe, mais paraissent aléatoires pour les besoins pratiques.

En Elixir, la fonction `:rand.uniform/1` utilise l'algorithme de génération de nombres aléatoires Mersenne Twister, qui est largement utilisé dans de nombreux langages de programmation.

## Voir aussi
- Documentation officielle d'Elixir sur la génération de nombres aléatoires: https://elixir-lang.org/getting-started/random-numbers.html
- Article "Génération de nombres aléatoires en Elixir" sur le blog de la communauté Elixir: https://elixir-lang.org/blog/2017/11/17/random-numbers-in-elixir/