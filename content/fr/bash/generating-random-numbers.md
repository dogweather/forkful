---
title:                "Génération de nombres aléatoires"
html_title:           "Bash: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

On peut avoir besoin de générer des nombre aléatoires dans une variété de cas de programmation tels que la simulation, le chiffrement ou la sécurité. Dans cet article, nous allons explorer comment générer des nombres aléatoires en utilisant Bash, le langage de script couramment utilisé sur les systèmes d'exploitation Unix.

## Comment faire

Pour générer des nombres aléatoires en utilisant Bash, nous allons utiliser la commande ```$RANDOM``` intégrée. Voici un exemple de code qui génère un nombre aléatoire entre 1 et 10 et l'affiche à l'écran:

```Bash
#!/bin/bash
num=$(($RANDOM % 10 + 1))
echo $num
```

La variable ```$RANDOM``` renvoie un nombre aléatoire compris entre 0 et 32767. En utilisant l'opérateur ```%``` (modulo), nous pouvons réduire cette plage de nombres pour obtenir un nombre aléatoire dans la plage souhaitée. Dans cet exemple, nous avons utilisé l'opérateur ```+``` pour déplacer la plage de nombres de 1 à 10 au lieu de 0 à 9. Enfin, la commande ```echo``` nous permet d'afficher le nombre aléatoire généré à l'écran.

## Plongez plus profondément

La commande ```$RANDOM``` est basée sur le générateur de nombres aléatoires pseudo-aléatoires. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais plutôt prévisibles en suivant une séquence. Cette séquence peut être modifiée en utilisant la variable d'environnement ```$RANDOM``` et en utilisant la commande ```RANDOM=$$``` avant l'utilisation de ```$RANDOM```. Cette méthode de modification de la séquence devrait être utilisée avec prudence, car cela peut affecter la génération de nombres aléatoires dans d'autres parties du script.

Vous pouvez également utiliser la commande ```shuf``` pour générer des nombres aléatoires dans une plage donnée. La syntaxe est la suivante:

```Bash
shuf -i [NOMBRE_MIN]-[NOMBRE_MAX] -n 1
```

Par exemple, pour générer un nombre aléatoire entre 1 et 10:

```Bash
shuf -i 1-10 -n 1
```

## Voir aussi

- [Manuel de Bash](https://www.gnu.org/software/bash/manual/)
- [Documentation de la commande shuf](https://man7.org/linux/man-pages/man1/shuf.1.html)
- [Article sur les générateurs de nombres aléatoires pseudo-aléatoires](https://medium.com/@sachinmore/generating-pseudo-random-numbers-with-bash-really-234371401c28)