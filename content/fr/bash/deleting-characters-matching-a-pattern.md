---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi ?

Supprimer des caractères selon un certain modèle, c'est éliminer des chaînes de caractères spécifiques dans un texte ou dans un script. Les programmeurs font cela pour nettoyer les données, normaliser les entrées ou simplifier les textes.

## Comment faire :

L'opération pour supprimer des caractères correspondant à un modèle se fait souvent avec 'tr' (translate) en Bash. Par exemple :

```Bash
echo "a1b2c3d4" | tr -d '1234'
```

Cette commande retourne une chaîne sans chiffres, soit "abcd". 

On peut aussi utiliser des modèles. Par exemple, si on veut supprimer toutes les lettres :

```Bash
echo "a1b2c3d4" | tr -d 'a-z'
```

Cette commande retourne "1234". 

## Plongée en profondeur :

Historiquement, 'tr' existe depuis les débuts d'UNIX et est un outil précieux pour les transformations de texte. Cependant, il existe d'autres méthodes pour supprimer des caractères correspondant à un modèle en Bash, notamment 'sed' et 'awk'. Par exemple, avec 'sed' :

```Bash
echo "a1b2c3d4" | sed 's/[1234]//g'
```

Cette commande donne le même résultat que notre premier exemple avec 'tr'. La différence majeure est que 'sed' et 'awk' sont beaucoup plus puissants et permettent des opérations de traitement de texte plus complexes.

L'implémentation de la suppression des caractères en Bash est assez directe. 'tr' lit les caractères un par un et les compare au modèle. Si un caractère correspond, il sera supprimé.

## Voir aussi :

Si vous voulez en savoir plus sur le sujet, consultez les liens suivants :

1. GNU 'tr' Manual : https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html#tr-invocation
2. Introduction à 'sed' : https://www.grymoire.com/Unix/Sed.html
3. Guide d'utilisation 'awk' : https://www.grymoire.com/Unix/Awk.html