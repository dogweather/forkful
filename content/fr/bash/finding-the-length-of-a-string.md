---
title:                "Bash: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant en Bash, vous pourriez vous demander pourquoi il est utile de trouver la longueur d'une chaîne de caractères. Eh bien, trouver la longueur d'une chaîne peut être très utile lors de la manipulation de données texte. Cela vous permettra de vérifier si une entrée utilisateur dépasse la limite de caractères autorisée ou de compter le nombre de caractères dans une certaine zone d'un texte.

## Comment faire

Trouver la longueur d'une chaîne en Bash est une tâche assez simple et peut être réalisée en quelques étapes.

D'abord, déclarez une variable contenant la chaîne que vous souhaitez vérifier. Par exemple :

```Bash
ma_variable="Bonjour le monde"
```

Ensuite, utilisez la commande `expr length` suivie de votre variable et entourez le tout de deux points d'exclamation. Par exemple :

```Bash
longueur=!!ma_variable
echo $longueur
```

Lorsque vous exécutez ce code, vous obtiendrez la sortie suivante :

```Bash
17
```

Cela indique que la chaîne contenue dans la variable `ma_variable` est composée de 17 caractères.

## Plongée en profondeur

Il est également possible de trouver la longueur d'une chaîne en utilisant la syntaxe du slicing en Bash. Cela peut être utile si vous souhaitez compter le nombre de caractères dans une certaine zone d'un texte.

Par exemple, si vous voulez trouver le nombre de caractères dans les 10 premiers caractères d'une chaîne, vous pouvez utiliser la syntaxe suivante :

```Bash
ma_variable="Bonjour le monde"
echo ${#ma_variable:0:10}
```

Cela vous donnera la sortie suivante :

```Bash
10
```

Cela signifie qu'il y a 10 caractères dans les 10 premiers caractères de la variable `ma_variable`.

## Voir aussi

- [La syntaxe des chaînes en Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [La commande `expr` en Bash](https://www.gnu.org/software/gcc/bash/manual/html_node/Shell-Arithmetic.html)