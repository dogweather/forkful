---
title:                "Bash: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

La génération de nombres aléatoires est un outil essentiel en programmation Bash. Elle permet de créer des applications plus dynamiques et interactives en ajoutant un élément imprévisible aux résultats. Cela peut être utile pour les jeux, les simulations ou tout simplement pour ajouter de l'intérêt à un programme.

## Comment faire

Pour générer des nombres aléatoires en Bash, nous pouvons utiliser la commande `shuf`. Voici un exemple de code pour générer un nombre aléatoire entre 1 et 10 :

```Bash
echo $((1 + $RANDOM % 10))
```

Le code ci-dessus utilise une méthode simple en utilisant `RANDOM`, une variable interne de Bash qui génère un nombre aléatoire entre 0 et 32767. Nous pouvons ensuite utiliser l'opérateur `%` pour limiter la plage et l'opérateur `+` pour spécifier à partir de quel nombre nous voulons commencer. Vous pouvez également modifier ces valeurs selon vos besoins.

Il est important de noter que `RANDOM` utilise une graine pseudo-aléatoire, donc si vous voulez obtenir des résultats vraiment aléatoires, vous devriez utiliser un ensemble de graines différentes à chaque exécution.

## Plongée en profondeur

Si vous souhaitez en savoir plus sur la génération de nombres aléatoires en Bash, il existe une commande intégrée appelée `$RANDOM` qui utilise une vraie source d'entropie pour générer des nombres aléatoires. Cela rend les résultats plus sûrs et plus aléatoires que la méthode précédente. Voici un exemple d'utilisation :

```Bash
echo $RANDOM
```

En utilisant cette méthode, aucun calcul n'est nécessaire et la plage de nombres est plus large, allant de 0 à 32767.

## Voir aussi

- La documentation officielle de Bash sur `shuf` : https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html
- Des exemples de code pour générer des nombres aléatoires en Bash : https://linuxhint.com/bash_random_numbers/