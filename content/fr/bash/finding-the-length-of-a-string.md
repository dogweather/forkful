---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Bash: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
De temps en temps, dans votre programme Bash, vous pourriez avoir besoin de savoir la longueur d'une chaîne, c'est-à-dire le nombre de caractères qu'elle contient. Cela peut être utile pour vérifier la validité d'une entrée utilisateur ou manipuler certaines données avant de les utiliser.

## Comment faire
Pour trouver la longueur d'une chaîne en Bash, vous pouvez utiliser la commande `expr length`. Il suffit de lui passer la chaîne en question entre guillemets et elle vous retournera le nombre de caractères.

```
Bash: expr length "Bonjour"
6
```

Vous pouvez également utiliser la commande `wc` (compteur de mots) avec l'option `-m` (pour spécifier le nombre de caractères) et lui passer comme argument la chaîne à mesurer.

```
Bash: echo "Bonjour" | wc -m
7
```

Notez que `wc` compte également le caractère de retour à la ligne, ce qui explique pourquoi l'output est différent de celui de `expr length`.

## Plongée plus profonde
Il y a une chose importante à garder à l'esprit lors de la mesure de la longueur d'une chaîne en Bash : la gestion des espaces. Si votre chaîne contient des espaces, ils seront également comptés dans la longueur.

Par exemple, si vous utilisez la commande `expr length` sur la chaîne `"Bonjour le monde"`, vous obtiendrez une valeur de 15 (y compris l'espace entre "Bonjour" et "le"). De même pour la commande `wc -m`, qui vous retournera 16 pour cette chaîne.

Cela peut être problématique si vous avez besoin de mesurer une chaîne sans tenir compte des espaces. Dans ce cas, vous devrez utiliser des expressions régulières ou des fonctions plus avancées pour traiter la chaîne et enlever les espaces avant de la mesurer.

## Voir aussi
- [Manipulation de chaînes en Bash](https://www.linuxjournal.com/content/bash-string-manipulation)
- [Guide rapide Bash](https://devhints.io/bash)