---
title:                "Extraction de sous-chaînes"
html_title:           "Bash: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Quand on programme en Bash, il arrive souvent qu'on ait besoin d'extraire des sous-chaînes de caractères à partir d'une chaîne plus longue. Cela peut être utile pour différents scénarios, comme la manipulation de données, la recherche de motifs dans du texte ou la création de scripts plus efficaces.

## Comment Faire

L'extraction de sous-chaînes en Bash se fait à l'aide de la commande `cut`. Voici un exemple de code avec une chaîne de caractères à extraire et une commande `cut` pour récupérer uniquement les 5 premiers caractères :

```Bash
# Chaîne de caractères à extraire
string="Bonjour tout le monde !"

# Commande cut pour extraire les 5 premiers caractères
cut -c1-5 <<< "$string"
```

La sortie de cette commande sera `Bonjour`, qui est la sous-chaîne de 5 premiers caractères de la chaîne initiale. On peut également utiliser `cut` pour extraire des caractères à partir d'un index spécifique, en utilisant l'option `-c` suivie de l'index de début et de fin, séparés par un tiret.

## Deep Dive

La commande `cut` peut également être utilisée pour extraire des sous-chaînes à partir d'un fichier en utilisant différentes options comme `-f` pour spécifier des champs délimités par un caractère spécifique, ou encore `-d` pour spécifier le délimiteur utilisé. Cette commande offre une grande flexibilité pour extraire des données à partir de sources variées.

Il est également possible d'utiliser la commande `awk` pour extraire des sous-chaînes en Bash. En utilisant le paramètre `substr`, on peut spécifier l'index de début et le nombre de caractères à extraire. Un exemple de code :

```Bash
# Chaîne de caractères à extraire
string="La programmation en Bash est amusante !"

# Commande awk pour extraire les 11 premiers caractères
awk '{print substr($0, 1, 11)}' <<< "$string"
```

La sortie sera `La programm`, qui est la sous-chaîne de 11 caractères à partir de l'index 1. La commande `awk` offre également des options pour extraire des sous-chaînes en utilisant des expressions régulières, ce qui peut être très utile pour des cas plus complexes.

## Voir Aussi

- [La documentation officielle de la commande `cut`](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html#cut-invocation)
- [La documentation officielle de la commande `awk`](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)