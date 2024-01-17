---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Bash: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Trouver la longueur d'une chaîne de caractères signifie simplement déterminer le nombre de caractères dans cette chaîne. Les programmeurs font cela pour plusieurs raisons, notamment pour manipuler et traiter des chaînes de manière efficace dans leurs programmes, ou pour valider la longueur de l'entrée de l'utilisateur.

## Comment faire :
Voici comment vous pouvez trouver la longueur d'une chaîne de caractères en utilisant Bash :

```Bash
# Déclarer une chaîne de caractères
my_string="Bonjour, monde!"

# Utiliser la commande "expr" avec l'option length
echo "La longueur de la chaîne est :" $(expr length "$my_string")

# Sortie : La longueur de la chaîne est : 14
```

Vous pouvez également utiliser la commande "wc" pour obtenir le même résultat :

```Bash
# Déclarer une autre chaîne de caractères
my_other_string="Ceci est une autre chaîne"

# Utiliser la commande "wc" avec l'option "c" pour compter les caractères
echo "La longueur de la chaîne est :" $(echo -n "$my_other_string" | wc -c)

# Sortie : La longueur de la chaîne est : 24
```

## Plongez plus en profondeur:
Dans les anciennes versions de Bash, pour trouver la longueur d'une chaîne, il fallait utiliser la commande "expr" avec l'option index pour compter le nombre de caractères dans une chaîne. Cependant, à partir de la version 4 de Bash, les options "length" et "index" ont été fusionnées, rendant ainsi les choses plus pratiques pour les programmeurs.

Une alternative à l'utilisation de la commande "wc" est d'utiliser la commande "echo" avec l'option "-n" pour supprimer les caractères de saut de ligne, car la commande "wc" compte également ceux-ci. Donc, si vous avez une chaîne de caractères qui contient également des sauts de ligne, cette méthode pourrait produire un résultat différent.

## Voir aussi:
Pour en savoir plus sur la manipulation de chaînes en Bash, vous pouvez consulter les documents officiels de Bash, ainsi que d'autres sources en ligne telles que Bash Academy et Stack Overflow.