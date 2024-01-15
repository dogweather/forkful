---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Fish Shell: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait utile de connaître la longueur d'une chaîne de caractères lors de la programmation en Fish Shell. Eh bien, c'est une information importante pour les manipulations et les vérifications des données, en particulier lors de l'écriture de scripts ou de commandes.

## Comment faire

En utilisant la commande intégrée `string length`, nous pouvons facilement trouver la longueur d'une chaîne de caractères. Voici un exemple de code et son résultat :

```Fish Shell
set maChaine "Bonjour!"
echo "La longueur de maChaine est" (string length $maChaine)
```

Résultat :

```
La longueur de maChaine est 9
```

## Deep Dive

Maintenant, intéressons-nous un peu plus à la commande `string length`. Cette commande renvoie simplement le nombre de caractères dans une chaîne de caractères donnée. Il prend en compte les espaces, les lettres majuscules et minuscules, ainsi que les caractères spéciaux. Par exemple, si nous donnons la chaîne de caractères "Hello, world!" à la commande, elle renverra 13 comme résultat, car il y a 13 caractères dans cette phrase.

Il est également important de noter que la commande `string length` ne peut être utilisée que sur des chaînes de caractères, et non sur des entiers ou d'autres types de données.

## Voir aussi

- [Documentation Fish Shell sur la commande `string length`](https://fishshell.com/docs/current/index.html#string-length)
- [Plus de détails sur les manipulations de chaînes de caractères en Fish Shell](https://www.linode.com/docs/guides/fish-string-manipulation/)