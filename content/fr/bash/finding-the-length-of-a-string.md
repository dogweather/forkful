---
title:                "Bash: Trouver la longueur d'une chaîne de caractères"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche de la longueur d'une chaîne de caractères est une tâche courante en programmation. Cela peut être utile lorsque vous devez manipuler des données de texte ou lorsque vous devez effectuer des vérifications de validation. Dans cet article, nous allons vous montrer comment trouver la longueur d'une chaîne de caractères en utilisant Bash.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Bash, nous utilisons la commande `expr length`. Cette commande prend une chaîne de caractères en paramètre et retourne sa longueur.

Voici un exemple d'utilisation de la commande `expr length`:

```Bash
$ myString="Bonjour"
$ echo "La longueur de la chaîne de caractères est : `expr length $myString`"
```

Lorsque vous exécutez ce code, vous obtiendrez le résultat suivant :

```Bash
La longueur de la chaîne de caractères est : 7
```

Vous pouvez également utiliser la commande `wc -c` pour trouver la longueur d'une chaîne de caractères. Cette commande compte le nombre de caractères d'un fichier ou d'une entrée donnée. Voici un exemple:

```Bash
$ myString="Bonsoir"
$ echo "La longueur de la chaîne de caractères est : `echo $myString | wc -c`"
```

Ce code renverra également `8` comme résultat.

## Plongée en profondeur

Il est important de garder à l'esprit que la longueur d'une chaîne de caractères peut varier selon la méthode utilisée pour la trouver. Par exemple, si votre chaîne de caractères contient des espaces, la commande `expr length` ne comptera pas ces espaces, tandis que la commande `wc -c` les comptera.

Il est également important de noter que la plupart des programmes de traitement de texte et d'autres langages de programmation ont leur propre méthode pour trouver la longueur d'une chaîne de caractères. Il est donc toujours utile de vérifier la documentation de votre programme pour trouver la méthode la plus appropriée.

En outre, si vous travaillez avec des caractères Unicode, la longueur d'une chaîne de caractères peut varier en fonction du type de codage utilisé. Par exemple, une chaîne de caractères Unicode peut être encodée en UTF-8, UTF-16 ou UTF-32, ce qui affectera la longueur de la chaîne. Vous devrez donc utiliser la méthode appropriée pour trouver la longueur en fonction de votre codage.

## Voir aussi

Voici quelques liens utiles pour en savoir plus sur la recherche de la longueur d'une chaîne de caractères en Bash :

- [La documentation officielle de `expr length`](https://ss64.com/bash/expr.html)
- [La documentation officielle de `wc`](https://ss64.com/bash/wc.html)
- [Les différentes façons de compter la longueur d'une chaîne en Bash](https://www.linuxjournal.com/content/bash-parameter-expansion)
- [Un guide complet sur le traitement de texte en Bash](https://www.linuxjournal.com/article/10715)