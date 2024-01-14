---
title:                "Ruby: Génération de nombres aléatoires"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Pourquoi Générer des Nombres Aléatoires en Ruby

Générer des nombres aléatoires est un outil puissant en programmation pour diverses raisons. Que ce soit pour des jeux, des simulations ou des tests, la capacité de créer des valeurs aléatoires peut être très utile dans de nombreux projets. Dans cet article, nous allons explorer comment utiliser Ruby pour générer des nombres aléatoires et plonger plus profondément dans le fonctionnement de ces nombres.

## Comment faire
Nous pouvons utiliser la méthode `rand` en Ruby pour générer des nombres aléatoires. La syntaxe de cette méthode est la suivante :

```Ruby
rand(max)
```

Ici, `max` représente la valeur maximum que nous voulons générer. Par exemple, si nous voulons générer un nombre aléatoire entre 1 et 10, nous utiliserons `rand(10)`. Voici un exemple complet de code :

```Ruby
puts rand(10)
puts rand(100)
```

Output:
```
6
73
```

Nous pouvons également utiliser `rand` avec des décimales en ajoutant un point décimal suivi du nombre de chiffres après la virgule. Par exemple, `rand(10.0)` générera un nombre aléatoire avec un chiffre après la virgule.

## Plongée plus profonde
En utilisant `rand` seul, nous obtenons des nombres aléatoires qui sont générés avec une distribution uniforme, c'est-à-dire que toutes les valeurs ont la même probabilité d'être générées. Cependant, en ajoutant un argument optionnel `seed`, nous pouvons influencer le flux de nombres aléatoires générés.

```Ruby
puts 'Sans seed :'
puts rand(10)
puts rand(10)

puts 'Avec seed :'
puts srand(123)
puts rand(10)
puts rand(10)
```

Output :
```
Sans seed :
6
5
Avec seed :
123
9
9
```

Comme vous pouvez le voir, en utilisant `srand` avec la même valeur `seed`, nous obtenons toujours les mêmes nombres aléatoires. Cette méthode est utile lorsque nous voulons reproduire des résultats pour des tests ou des simulations.

## Voir Aussi
- [La documentation officielle de la méthode rand en Ruby](https://ruby-doc.org/core-2.6/Kernel.html#method-i-rand)
- [Une explication détaillée sur la génération de nombres aléatoires en Ruby](https://www.sitepoint.com/random-numbers-in-ruby-an-overview/)

En utilisant la méthode `rand` en conjonction avec `srand`, nous pouvons facilement générer des nombres aléatoires en Ruby pour une variété de projets. J'espère que cet article vous a été utile et que vous êtes maintenant bien équipé pour ajouter des nombres aléatoires à vos futurs projets en Ruby.