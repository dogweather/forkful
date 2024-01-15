---
title:                "Supprimer les caractères correspondant à un motif"
html_title:           "Fish Shell: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile dans certaines situations lors de l'utilisation de Fish Shell. Par exemple, vous pouvez vouloir supprimer tous les emojis d'un texte ou toutes les lettres majuscules d'un nom de fichier.

## Comment faire

Pour supprimer des caractères correspondant à un modèle, utilisez la commande `string replace` en indiquant le modèle à rechercher et le caractère de remplacement entre guillemets. Par exemple, pour supprimer tous les emojis d'un texte, vous pouvez utiliser la commande suivante :

```Fish Shell
set texte "Bonjour 🌞 comment ça va 🤔"
set nouveau_texte (string replace -- "🌞" "" $texte)
```

La commande `string replace` renvoie une nouvelle chaîne de caractères avec le modèle remplacé par le caractère de remplacement. Dans cet exemple, le nouveau texte sera "Bonjour comment ça va".

## Approfondissement

La commande `string replace` accepte également des options pour effectuer des remplacements plus spécifiques. Par exemple, vous pouvez utiliser l'option `--all` pour remplacer toutes les occurrences du modèle au lieu de seulement la première. Vous pouvez aussi utiliser l'option `--ignore-case` pour ignorer la casse lors de la recherche du modèle.

De plus, la commande `string replace` peut être combinée avec d'autres commandes telles que `grep` ou `sed` pour effectuer des remplacements plus complexes. Vous pouvez également utiliser des expressions régulières pour un contrôle plus précis sur le modèle à rechercher.

## Voir aussi

- Documentation officielle de la commande [`string replace`](https://fishshell.com/docs/current/cmds/string-replace.html) 
- Stack Overflow [réponse](https://stackoverflow.com/questions/59895/how-to-remove-the-emoji-code-from-a-string-in-python) sur la suppression des emojis avec Fish Shell 
- Article [Medium](https://medium.com/@g7r/color-manipulation-in-fish-shell-a141b8afcfc1) sur la manipulation de couleurs avec Fish Shell.