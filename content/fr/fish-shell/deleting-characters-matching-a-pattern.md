---
title:                "Suppression de caractères correspondant à un modèle"
html_title:           "Fish Shell: Suppression de caractères correspondant à un modèle"
simple_title:         "Suppression de caractères correspondant à un modèle"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Lorsque vous programmez, il peut parfois arriver que vous souhaitiez supprimer des caractères spécifiques dans votre code. C'est là qu'intervient la fonction de suppression de caractères par motif. En utilisant cette fonction, vous pouvez facilement supprimer tous les caractères qui correspondent à un motif donné.

Les programmeurs utilisent cette fonction pour nettoyer leur code et le rendre plus lisible et facile à comprendre. En supprimant les caractères inutiles, ils peuvent se concentrer sur l'essentiel et écrire un code plus efficace.

## Comment faire:

Pour supprimer des caractères correspondant à un motif dans le Fish Shell, utilisez la commande `string replace`. Par exemple, si vous souhaitez supprimer tous les tirets d'un numéro de téléphone, vous pouvez utiliser la commande suivante:

```
fish_shell> string replace 555-1234 - ''
5551234 
```

Vous pouvez également utiliser un motif plus complexe en utilisant des expressions régulières. Par exemple, pour supprimer tous les chiffres d'une chaîne de caractères, vous pouvez utiliser la commande suivante :

```
fish_shell> string replace Hello123 my_pattern ''
Hello 
```

La fonction de suppression de caractères par motif utilise également des options telles que `--all` qui permet de supprimer toutes les occurrences du motif dans la chaîne.

## Plongée en profondeur:

Cette fonction est inspirée de la fonction `sed` de UNIX, qui permet également de supprimer des caractères correspondant à un motif. Dans d'autres shells, vous pouvez utiliser la commande `tr` pour effectuer cette tâche.

L'utilisation de l'expression régulière permet une plus grande souplesse dans la sélection des caractères à supprimer. Cependant, il est important de noter que cela peut également rendre la fonction plus complexe à utiliser.

## Voir également:

- [Documentation Fish Shell](http://fishshell.com/docs/current/cmds/string.html#string-replace)

- [Explications sur les expressions régulières](https://www.regular-expressions.info/)