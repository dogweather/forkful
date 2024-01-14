---
title:                "Fish Shell: Suppression de caractères correspondants à un modèle"
simple_title:         "Suppression de caractères correspondants à un modèle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondants à un modèle peut être une tâche courante lors de la programmation en Shell Fish. Cela peut être utile pour nettoyer des données, supprimer des espaces inutiles ou simplement pour formater des informations.

## Comment faire

Pour supprimer des caractères correspondants à un modèle dans le Shell Fish, vous pouvez utiliser la commande `string replace`, en spécifiant le modèle de caractères à supprimer ainsi que le nouveau modèle à utiliser. Par exemple, pour remplacer tous les espaces par des tirets dans une chaîne de caractères, vous pouvez utiliser la commande suivante :

```Fish Shell
set my_string "Ceci est un exemple"
string replace " " "-" $my_string
```
Sortie : `Ceci-est-un-exemple`

## Approfondissement

La commande `string replace` peut être utilisée avec des expressions régulières pour supprimer des caractères complexes. Par exemple, pour supprimer tous les chiffres d'une chaîne de caractères, vous pouvez utiliser l'expression régulière `[0-9]` dans la commande. Il est également possible de combiner plusieurs commandes `string replace` pour supprimer plusieurs motifs à la fois.

## Voir aussi

- [Documentation du Shell Fish](https://fishshell.com/docs/current/)
- [Guide de référence du Shell Fish](https://fishshell.com/docs/current/cmds/string-replace.html)
- [Guide complet de l'expression régulière en Shell Fish](https://codereviewvideos.com/blog/fish-shell/regular-expressions-in-fish-shell/)