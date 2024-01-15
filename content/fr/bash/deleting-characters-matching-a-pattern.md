---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "Bash: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir supprimer des caractères correspondant à un modèle dans un code Bash. Cela peut être utile pour nettoyer des données, effectuer des transformations de texte, ou pour tout autre besoin spécifique dans votre script.

## Comment faire

Pour supprimer des caractères correspondant à un modèle dans un code Bash, vous pouvez utiliser la commande "sed" suivie de l'expression régulière du modèle à supprimer.

```
#!/bin/bash

# Supprimer tous les caractères "a" d'une chaîne
string="abracadabra"
echo $string | sed 's/a//g'
# Output: brcdbr

# Supprimer les chiffres d'une chaîne
string="abc123def456"
echo $string | sed 's/[0-9]//g'
# Output: abcdef
```

Vous pouvez également utiliser la commande "tr" pour supprimer des caractères en utilisant un modèle inverse. Par exemple, pour supprimer toutes les lettres sauf les voyelles, vous pouvez utiliser la commande suivante:

```
#!/bin/bash

# Supprimer toutes les lettres sauf les voyelles
string="hello world"
echo $string | tr -cd '[aeiou]'
# Output: eo o
```

## Plongez plus profondément

La commande "sed" est très utile pour supprimer des caractères en utilisant des expressions régulières. Vous pouvez en apprendre plus sur son fonctionnement en consultant la documentation officielle ici: https://www.gnu.org/software/sed/manual/sed.html.

De plus, vous pouvez également utiliser la commande "awk" pour supprimer des caractères en utilisant des modèles plus complexes. Vous pouvez en savoir plus sur cette commande en consultant la documentation officielle ici: https://www.gnu.org/software/gawk/manual/gawk.html.

## Voir aussi

- La documentation complète pour la commande "sed": https://www.gnu.org/software/sed/manual/sed.html
- La documentation complète pour la commande "awk": https://www.gnu.org/software/gawk/manual/gawk.html