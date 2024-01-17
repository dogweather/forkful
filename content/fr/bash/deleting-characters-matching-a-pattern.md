---
title:                "Suppression des caractères correspondants à un motif"
html_title:           "Bash: Suppression des caractères correspondants à un motif"
simple_title:         "Suppression des caractères correspondants à un motif"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

Supprimer des caractères correspondant à un modèle est une fonctionnalité utile en programmation Bash qui permet de filtrer des données et de manipuler des chaînes de caractères. Les programmeurs l'utilisent souvent pour nettoyer une entrée utilisateur ou pour extraire des informations spécifiques d'un fichier texte.

## Comment faire:

Voici quelques exemples de code pour illustrer comment supprimer des caractères correspondant à un modèle en utilisant la commande ```Bash sed```. Ce code doit être exécuté dans un terminal Bash.

Suppression des lettres "u" et "t" des mots:

```Bash
echo "cet été est trop chaud" | sed 's/[ut]//g'```
Output: "ce été es rop cha"

Supprimer les nombres de 0 à 9 :

```Bash
echo "abc123def" | sed 's/[0-9]//g'```
Output: "abcdef"

## Plongée en profondeur:

Cette fonctionnalité est inspirée de la commande Unix "sed" qui a été développée en 1973. Elle est basée sur l'expression régulière et peut être utilisée pour effectuer des opérations plus complexes telles que la substitution de texte. D'autres alternatives, telles que la commande "tr" ou les expressions régulières en ligne de commande, peuvent également être utilisées pour supprimer des caractères correspondant à un modèle. La suppression de caractères est réalisée en parcourant la chaîne de caractères et en utilisant un script Java pour effectuer des remplacements en fonction du modèle fourni.

## Voir aussi:

Pour en savoir plus sur la commande "sed" et les expressions régulières en Bash:
- Documentation officielle de Bash: https://www.gnu.org/software/bash/manual/bash.html
- Tutoriel sur les expressions régulières en ligne de commande: https://www.linuxjournal.com/article/10817