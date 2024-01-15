---
title:                "Convertir une chaîne en minuscules"
html_title:           "Bash: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous manipulez beaucoup de chaînes de caractères en Bash, il peut être utile de les convertir en minuscules pour faciliter les comparaisons et les recherches.

## Comment faire
Pour convertir une chaîne de caractères en minuscules en Bash, utilisez la commande `tr` (translate) avec le drapeau `-s` (squeeze) pour supprimer toutes les occurrences consécutives de caractères en minuscules.

```Bash
# Exemple d'une chaîne de caractères en majuscules
string="SALUT TOUT LE MONDE"

# Utilisation de la commande tr pour la convertir en minuscules
echo "$string" | tr -s '[A-Z]' '[a-z]'

# Sortie: salut tout le monde
```

## Plongée en profondeur
Il est important de noter que la commande `tr` ne modifie pas la valeur de la variable originale, elle renvoie simplement le résultat de la conversion. De plus, vous pouvez spécifier une plage de caractères à convertir en minuscules en utilisant des intervalles, par exemple `[A-Z]` pour toutes les lettres majuscules de l'alphabet.

## Voir aussi
- Documentation officielle de la commande `tr` en Bash : https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation
- Tutoriel vidéo sur la manipulation de chaînes de caractères en Bash : https://www.youtube.com/watch?v=khPdAlPkoqg