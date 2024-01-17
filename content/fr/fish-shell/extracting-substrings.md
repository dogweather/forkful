---
title:                "Extraction de sous-chaînes"
html_title:           "Fish Shell: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

L'extraction de sous-chaînes dans la programmation, c'est simplement le fait de récupérer une partie d'une chaîne de caractères. Les programmeurs le font souvent pour obtenir des informations spécifiques d'une chaîne, tels que des noms de fichiers ou des adresses URL.

## Comment faire:

```
Fish Shell a une commande intégrée pour extraire des sous-chaînes: `string`. Par exemple, si nous avons la chaîne "Bonjour tout le monde", nous pouvons extraire "tout" en utilisant la commande suivante:

```fish
set my_string "Bonjour tout le monde"
string sub $my_string 8 4
```

Cela retournera simplement "tout". Vous pouvez également extraire une sous-chaîne en utilisant un motif, comme dans l'exemple suivant:
```fish
set my_string "hello_world_123"
string match *world* $my_string
```

Cela retournera "world". Vous pouvez également extraire une sous-chaîne à partir de la fin de la chaîne en utilisant un indice négatif. Par exemple:
```fish
set my_string "Bonjour tout le monde"
string sub $my_string -6 -1
```
Cela retournera "monde".

## Deep Dive:

L'extraction de sous-chaînes peut sembler une tâche simple, mais elle a une grande importance dans la programmation. Cela permet aux programmeurs de manipuler facilement des chaînes de caractères et d'obtenir des informations précises. Il existe également différentes façons d'extraire des sous-chaînes dans d'autres langages de programmation, tels que Python ou Java. Dans Fish Shell, la commande `string` utilise la syntaxe commune du langage de programmation C pour définir le début et la fin de la sous-chaîne. Cela permet une utilisation familière pour les programmeurs qui passent de C à Fish Shell.

## Voir aussi:

- [Guide officiel de Fish Shell sur la commande "string"](https://fishshell.com/docs/current/cmds/string.html)
- [Exemples d'utilisation de l'extraction de sous-chaînes en Python](https://www.guru99.com/python-regular-expressions-complete-tutorial.html#6)
- [Tutoriel sur la manipulation de chaînes en Java](https://www.dummies.com/programming/java/how-to-extract-substrings-from-a-string-in-java/)