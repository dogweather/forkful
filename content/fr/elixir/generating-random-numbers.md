---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Générer des nombres aléatoires est une façon de produire des chiffres sans ordre ou sans modèle prévisible. Les programmeurs le font pour des raisons variées : simulations, jeux, tests, et plus encore.

## Comment faire:
Voici comment générer un nombre aléatoire simple en Elixir.
```Elixir 
IO.inspect(:rand.uniform(10)) 
```

Quand on exécute ce programme, on obtient une sortie comme ceci : 
```Elixir 
7
```

Pour des nombres à virgule, essayez ceci:
```Elixir 
IO.inspect(:rand.uniform()) 
```

Output :
```Elixir 
0.5641242522238273
```

## Plongée profonde
Historiquement, générer des nombres aléatoirement était difficile et pas toujours parfait. Aujourd'hui, Elixir utilise l'algorithme de Mersenne Twister pour générer du faux aléatoire, qui est presque aussi bon que le vrai aléatoire.

Il y a d'autres façons de générer des nombres aléatoires en Elixir. Par exemple, la librairie 'exs1024' offre une alternatif aux fonctions intégrées :rand.

Il faut savoir que :rand.uniform() en Elixir retourne un nombre aléatoire de l'intervalle [0.0, 1.0). Notez que 1.0 est exclu.

## Voir aussi
- [ElixirDocs - :rand](https://hexdocs.pm/elixir/Kernel.html#inspect/2)
- [Librairie 'exs1024'](https://github.com/jj1bdx/exs1024/)
- [Algorithme de Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)