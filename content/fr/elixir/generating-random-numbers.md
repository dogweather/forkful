---
title:                "Elixir: Générer des nombres aléatoires"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler un exercice inutile pour certains, mais c'est une pratique importante en programmation pour diverses raisons. Cela peut être utile pour simuler des données dans un environnement contrôlé, tester des algorithmes ou même ajouter un élément de surprise dans les jeux ou les applications.

## Comment faire

Pour générer des nombres aléatoires en Elixir, nous utilisons la fonction `:rand.uniform/1`. Cette fonction prend en paramètre une liste, un tuple ou un nombre et renvoie un nombre aléatoire compris dans la plage spécifiée. Par exemple, pour générer un nombre aléatoire entre 1 et 10, nous pouvons utiliser la fonction de cette manière :

```Elixir
:rand.uniform(1..10) # renvoie un nombre aléatoire entre 1 et 10
```

Nous pouvons également générer des nombres aléatoires à virgule en spécifiant une plage de nombres décimaux. Par exemple, pour générer un nombre aléatoire entre 0 et 1, nous pouvons utiliser la fonction de cette manière :

```Elixir
:rand.uniform(0.0..1.0) # renvoie un nombre décimal aléatoire entre 0 et 1
```

Nous pouvons également utiliser la fonction `:rand.uniform/0` pour générer un nombre aléatoire à virgule de précision infinie dans la plage `[0, 1)`.

## Plongée en profondeur

Lors de la génération de nombres aléatoires en Elixir, il est important de comprendre comment la fonction `:rand.uniform/1` travaille en interne. En fait, cette fonction utilise une autre fonction appelée `:rand.uniform_s/1` qui utilise un algorithme appelé "Mersenne Twister". Cet algorithme est très efficace pour générer des nombres aléatoires et est utilisé dans de nombreux langages de programmation.

Il est également important de noter que la fonction `:rand.uniform/1` utilise un générateur de nombre aléatoire pseudo-aléatoire, ce qui signifie que les mêmes entrées produiront toujours les mêmes sorties. Cela peut être utile pour la reproductibilité, mais si vous avez besoin de générer des nombres vraiment aléatoires, vous pouvez utiliser la fonction `:rand.seed/1` pour initialiser le générateur avec une "graine" différente à chaque fois.

## Voir aussi

- Documentation officielle sur la fonction `:rand.uniform/1` : https://hexdocs.pm/elixir/Kernel.SpecialForms.html#random-generators

- Tutoriel sur la génération de nombres aléatoires en Elixir : https://www.codementor.io/@ostapio/randomness-with-elixir-e9mcv119s

- Utilisations pratiques de la génération de nombres aléatoires en Elixir : https://dashbit.co/blog/randomness-and-shuffle-in-elixir