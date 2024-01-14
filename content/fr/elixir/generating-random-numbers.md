---
title:                "Elixir: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi 
 Générer des nombres aléatoires est un outil essentiel pour de nombreux programmeurs. Cela permet de simuler des situations aléatoires et d'ajouter du chaos contrôlé dans vos projets informatiques. Dans cet article, nous allons plonger dans la génération de nombres aléatoires en utilisant Elixir.

## Comment Faire 
Nous allons maintenant voir comment générer des nombres aléatoires en utilisant Elixir. Tout d'abord, nous devons importer le module Kernel qui contient plusieurs fonctions utiles pour la manipulation de données aléatoires.

```Elixir 
iex> import Kernel
```
Ensuite, nous pouvons utiliser la fonction `:random.uniform/1` pour générer un nombre aléatoire dans une plage donnée. Par exemple, pour générer un nombre aléatoire entre 1 et 100, nous pouvons utiliser :

```Elixir
iex> :random.uniform(1..100)
92
```
Nous pouvons également utiliser la fonction `:random.seed/1` pour initialiser une graine aléatoire, ce qui garantit que les nombres générés seront différents à chaque exécution de notre programme.

```Elixir 
iex> :random.seed(:os.system_time(:millisecond))
:ok
```
Pour générer plusieurs nombres aléatoires en une seule fois, nous pouvons utiliser la fonction `Enum.map/2` qui nous permet d'appliquer une fonction sur une liste de valeurs. Par exemple, pour générer une liste de 10 nombres aléatoires entre 1 et 100 :

```Elixir
iex> Enum.map(1..10, fn _ -> :random.uniform(1..100) end)
[40, 76, 30, 94, 17, 52, 86, 25, 78, 2]
```

## Plongée en profondeur
Maintenant que nous avons vu comment générer des nombres aléatoires en utilisant Elixir, il est important de comprendre comment cela fonctionne réellement. En informatique, il est impossible de générer un nombre complètement aléatoire, car les ordinateurs suivent des algorithmes déterministes. Cependant, en utilisant une graine aléatoire et des opérations mathématiques complexes, nous pouvons simuler des nombres aléatoires de haute qualité.

Il est également important de noter que les nombres générés par les fonctions `:random.uniform/1` et `:random.seed/1` sont en fait des nombres à virgule flottante. Cela signifie qu'il peut y avoir une certaine imprécision dans les résultats. Si une précision exacte est nécessaire, il faut utiliser la fonction `:random.uniform/2` qui accepte un type de données spécifique (integers, floats, etc.).

## Voir aussi
Pour en savoir plus sur la génération de nombres aléatoires en Elixir, vous pouvez consulter ces ressources :

- [Documentation officielle d'Elixir sur les nombres aléatoires](https://hexdocs.pm/elixir/Kernel.html#random.uniform/1)
- [Article de Medium sur la génération de nombres aléatoires en Elixir](https://medium.com/@dazuma/generating-random-numbers-with-elixir-9234c9aba2f4)
- [Exemple de projet GitHub utilisant des nombres aléatoires en Elixir](https://github.com/robyurkowski/pitch-role-generator/blob/master/lib/pitch_role_generator.ex)

Merci d'avoir lu cet article sur la génération de nombres aléatoires en Elixir ! N'hésitez pas à explorer et à expérimenter avec ces fonctions dans vos propres projets. À bientôt !