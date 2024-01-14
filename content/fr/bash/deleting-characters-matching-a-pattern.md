---
title:                "Bash: Suppression des caractères correspondant à un motif"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile dans une multitude de cas en programmation Bash. Cela permet de nettoyer du texte, de modifier des variables ou de filtrer des données pour lesquelles un motif spécifique est recherché.

## Comment faire

Pour supprimer des caractères correspondant à un motif en Bash, vous pouvez utiliser la commande `sed`. Voici un exemple de code qui supprime tous les caractères numériques d'une chaîne de caractères :

```Bash
echo "123abc456" | sed 's/[0-9]//g'
```

L'output de ce code sera `abc`. La commande `sed` prend en paramètre l'expression régulière `[0-9]` pour cibler les chiffres et le flag `g` pour supprimer toutes les occurrences de ce motif.

## Plongée en profondeur

La commande `sed` offre de nombreuses possibilités pour supprimer des caractères correspondant à un motif. Vous pouvez utiliser des expressions régulières plus complexes pour cibler des motifs plus spécifiques ou combiner `sed` avec d'autres commandes comme `grep` pour filtrer des données en fonction d'un motif précis. De plus, il est également possible de créer des fichiers de substitution avec `sed` pour remplacer des caractères par d'autres.

## Voir aussi

- [Utilisation de sed pour supprimer des caractères correspondant à un motif](https://www.cyberciti.biz/faq/how-to-delete-character-from-file-using-sed-is-it-possible/)
- [Guide complet sur les expressions régulières en Bash](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)
- [Documentation officielle de la commande sed](https://www.gnu.org/software/sed/manual/sed.html)