---
title:                "Fish Shell: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires est une tâche courante dans la programmation, que ce soit pour créer des jeux, des simulations ou pour tester des applications. Dans cet article, nous allons découvrir comment le faire facilement en utilisant le shell Fish.

## Comment faire

Pour générer des nombres aléatoires dans Fish Shell, nous pouvons utiliser la commande ```math``` en combinaison avec la fonction ```rand()```. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 100 :

```
math rand -r '(1..100)' 
```

Si vous exécutez cette commande plusieurs fois, vous remarquerez que le nombre généré change à chaque fois.

Pour générer un nombre aléatoire avec une plage spécifique, vous pouvez utiliser la syntaxe ```(min..max)```. Par exemple, pour générer un nombre entre -10 et 10, vous pouvez utiliser cette commande :

```
math rand -r '(-10..10)' 
```

Il est également possible de générer des nombres aléatoires avec des décimales en utilisant la syntaxe ```(min..max, step)```. Par exemple, pour générer un nombre avec une décimale entre 0 et 1, vous pouvez utiliser cette commande :

```
math rand -r '(0..1, .1)' 
```

## Plongée en profondeur

La commande ```math``` est en fait une interface pour accéder aux fonctions mathématiques du langage C. La fonction ```rand()``` en fait partie, et elle utilise un algorithme pour générer des nombres pseudo-aléatoires à partir d'une valeur de départ appelée "graine" (seed en anglais).

La graine par défaut pour la fonction ```rand()``` est généralement basée sur l'horloge interne de l'ordinateur, ce qui signifie qu'elle change souvent. Cela explique pourquoi les nombres générés changent à chaque exécution.

## Voir aussi

- [Documentation sur la commande ```math```](https://fishshell.com/docs/current/cmds/math.html)
- [Documentation sur la fonction ```rand()```](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Article sur la génération de nombres aléatoires en Shell Bash](https://www.linuxjournal.com/content/random-numbers-bash)