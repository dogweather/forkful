---
title:    "Elixir: Génération de nombres aléatoires"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler être une tâche simple et peu utile à première vue. Cependant, en programmation, il existe de nombreuses situations où avoir la possibilité de générer des nombres de manière aléatoire peut être très utile. Que ce soit pour simuler des scénarios, créer des tests ou pour des jeux, la génération de nombres aléatoires est une compétence essentielle à maîtriser en programmation.

## Comment faire

La programmation en Elixir offre différentes façons de générer des nombres aléatoires. La méthode la plus simple consiste à utiliser la fonction `:random.uniform/1` qui accepte un nombre entier en guise de limite et renvoie un nombre aléatoire compris entre 0 et cette limite. Par exemple :

```Elixir
:random.uniform(10)
```

produirait un nombre aléatoire entre 0 et 10, inclusivement.

Pour générer un nombre aléatoire dans une plage spécifique, on peut utiliser la fonction `:random.uniform/2`. Cette fonction accepte une limite inférieure et une limite supérieure et renvoie un nombre aléatoire compris entre ces deux limites. Par exemple :

```Elixir
:random.uniform(5, 10)
```

produirait un nombre aléatoire entre 5 et 10.

Il est également possible de générer des nombres aléatoires avec des types de données spécifiques, tels que des nombres en virgule flottante ou des atomes. Pour cela, on peut utiliser les fonctions `:random.uniform_float/0` et `:random.uniform_atom/0`.

## Plongée en profondeur

En plus de générer des nombres aléatoires, il est également possible de contrôler la séquence de nombres générés en utilisant la fonction `:random.seed/1`. Cette fonction prend en argument une valeur et initialise le générateur de nombres aléatoires avec cette valeur, ce qui permet de reproduire la même séquence de nombres aléatoires si nécessaire.

Il est également possible d'utiliser un module spécifique pour la génération de nombres aléatoires en utilisant la fonction `:random.seed/2`. Cette fonction prend en argument un module et une valeur et utilise cette valeur comme graine pour générer des nombres aléatoires spécifiques au module donné.

Enfin, pour des applications nécessitant une sécurité plus élevée, il est recommandé d'utiliser le module `:crypto` qui fournit des fonctions pour générer des nombres aléatoires cryptographiquement sécurisés.

## Voir aussi

Pour en savoir plus sur la génération de nombres aléatoires en Elixir, consultez les ressources suivantes :

- La documentation officielle d'Elixir sur les fonctions de génération de nombres aléatoires : https://hexdocs.pm/elixir/Kernel.html#random?/1
- Un article détaillé sur les différentes méthodes de génération de nombres aléatoires en Elixir : https://blog.lelonek.me/random-numbers-in-elixir-8d05f3c98e19
- Un exemple d'utilisation de générateurs de nombres aléatoires en jeu avec Elixir : https://itnext.io/building-a-simple-game-in-elixir-a1332b7ea4b4