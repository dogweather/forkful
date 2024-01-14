---
title:                "Fish Shell: Concaténer des chaînes de caractères"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est une pratique courante dans la programmation, et elle peut être très utile dans certaines situations. Cela permet de combiner plusieurs chaînes de caractères en une seule, ce qui peut être pratique pour la manipulation de données ou l'affichage de messages.

## Comment faire

Pour concaténer des chaînes de caractères en utilisant Fish Shell, il suffit d'utiliser l'opérateur de concaténation "+". Par exemple:

```
Fish Shell

set nom "Marie"
set nom_complet $nom "Dupont"
echo $nom_complet
```

Cela va d'abord créer une variable "nom" contenant la valeur "Marie", puis concaténer cette valeur avec "Dupont" en utilisant l'opérateur "+". Le résultat de l'exécution du code sera "Marie Dupont".

## Plongée en profondeur

La concaténation de chaînes de caractères peut également être réalisée avec la fonction "string join". Cette fonction prend en paramètres un délimiteur et une liste de chaînes de caractères à concaténer. Elle peut être utile lorsque l'on souhaite concaténer plusieurs chaînes de caractères contenues dans une liste. Voici un exemple:

```
Fish Shell

set fruits ("banane" "pomme" "orange")
set liste_fruits (string join ", " $fruits)
echo "Mes fruits préférés sont: "$liste_fruits
```

Le résultat de l'exécution de ce code sera "Mes fruits préférés sont: banane, pomme, orange".

## Voir aussi

- Documentation officielle de Fish Shell sur la concatenation de chaînes de caractères: https://fishshell.com/docs/current/cmds/set.html#string-join
- Tutoriel sur la manipulation de chaînes de caractères en Fish Shell: https://www.shell-tips.com/fish/shell-scripting-concatenate-strings/