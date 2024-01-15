---
title:                "Génération de nombres aléatoires"
html_title:           "Elixir: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait pertinent de générer des nombres aléatoires en programmation. Eh bien, cela peut être utile pour simuler des situations aléatoires, créer des jeux, ou encore pour générer des données aléatoires pour des tests.

## Comment faire

Pour générer des nombres aléatoires en Elixir, nous pouvons utiliser la fonction `:random.uniform/0`de la bibliothèque standard. Cette fonction renvoie un nombre décimal compris entre 0 inclus et 1 exclus.

```
Elixir

:random.uniform()

0.8326243409998113
```

Si nous voulons générer un nombre aléatoire dans un intervalle donné, nous pouvons utiliser la fonction `:random.uniform/2` en lui passant en paramètres l'intervalle voulu.

```
Elixir

:random.uniform(10, 20)

15.457681015782368
```

De plus, nous pouvons utiliser des fonctions de la bibliothèque standard telles que `:random.seed/1` ou `:random.seed/2` pour initialiser le générateur de nombres aléatoires avec une valeur de départ.

## Plongée profonde

Il est important de noter que la fonction `:random.uniform/0` utilise un générateur de nombres pseudo-aléatoires basé sur un algorithme déterministe. Cela signifie que si nous utilisons la même graine (seed) pour initialiser le générateur, nous obtiendrons la même séquence de nombres aléatoires à chaque fois.

Nous pouvons également utiliser la bibliothèque `:rand` pour générer des nombres aléatoires avec plus de flexibilité, comme par exemple pour choisir un élément aléatoire dans une liste.

## Voir aussi

- [Documentation Elixir officielle sur les nombres aléatoires](https://hexdocs.pm/elixir/Random.html)
- [Article sur la génération de nombres aléatoires en Elixir](https://www.cogini.com/blog/generating-random-numbers-with-elixir/)