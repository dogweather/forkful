---
title:                "Génération de nombres aléatoires"
html_title:           "Fish Shell: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

Pourquoi & Comment? 

Générer des nombres aléatoires est un processus où nous pouvons produire des nombres aléatoires sur notre ordinateur. Les programmeurs le font souvent pour simuler des situations aléatoires ou pour ajouter de l'imprévisibilité aux programmes.

Comment faire? 

Utilisons Fish Shell pour générer des nombres aléatoires! Dans l'exemple ci-dessous, nous utiliserons la commande ```math (random)`` pour générer un nombre aléatoire entre 0 et 1. 

```
Fish Shell

math (random)

Output: 0.426902
```

Pour générer un nombre aléatoire entre un minimum et un maximum spécifié, nous pouvons utiliser la commande ```random``` suivie du minimum et du maximum séparés par un espace. Dans cet exemple, nous allons générer un nombre aléatoire entre 1 et 10. 

```
Fish Shell

random 1 10

Output: 7
```

plongée en profondeur 

La génération de nombres aléatoires est un concept important en informatique et a un usage courant dans de nombreux domaines tels que les jeux, les simulations et la cryptographie. Avant l'invention des ordinateurs, les nombres aléatoires étaient générés en utilisant des phénomènes aléatoires tels que le lancer de dés ou la roue de la roulette. Cependant, avec les ordinateurs, nous pouvons générer des nombres aléatoires de manière plus efficace et précise. 

Alternatives possibles 

Outre la méthode que nous avons utilisée avec Fish Shell, il existe d'autres façons de générer des nombres aléatoires en utilisant d'autres langages de programmation tels que Python, C et Java. Cependant, ces langages peuvent être plus verbeux et nécessitent parfois l'importation de bibliothèques supplémentaires. Avec Fish Shell, c'est une solution simple et rapide pour générer des nombres aléatoires. 

Voir aussi 

Pour en savoir plus sur la génération de nombres aléatoires avec Fish Shell, vous pouvez consulter la page man de Fish et découvrir d'autres commandes utiles telles que ```random```, ```pick``` et ```seq```. Vous pouvez également consulter des tutoriels en ligne pour découvrir des façons créatives d'utiliser ces commandes dans vos programmes.