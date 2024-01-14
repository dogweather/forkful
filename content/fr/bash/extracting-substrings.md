---
title:                "Bash: Extraction de sous-chaines"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi 

Extrayer des sous-chaînes de caractères est une compétence utile à connaître pour tout programmeur Bash. Cela vous permet de manipuler et de modifier efficacement des chaînes de caractères, ce qui peut être utile pour diverses tâches de programmation.

## Comment faire

Pour extraire une sous-chaîne de caractères dans Bash, vous pouvez utiliser la commande `cut`. Par exemple, si vous avez une chaîne de caractères "Bonjour le monde", et que vous voulez extraire "le monde", vous pouvez utiliser la commande suivante :

```Bash
echo "Bonjour le monde" | cut -d' ' -f2-
```

Cela utilise le délimiteur d'espace pour séparer la chaîne en deux parties, puis utilise le sélecteur `-f2-` pour sélectionner la deuxième partie jusqu'à la fin.

Vous pouvez également utiliser des expressions régulières pour extraire des sous-chaînes de manière plus précise. Par exemple, si vous voulez extraire tous les chiffres d'une chaîne de caractères, vous pouvez utiliser la commande suivante :

```Bash
echo "J'ai 5 pommes" | grep -o '[0-9]*'
```

Cela sélectionnera et affichera uniquement les chiffres de la chaîne "5". 

## Plongeon profond

Il existe de nombreuses autres façons d'extraire des sous-chaînes de caractères en utilisant Bash. Vous pouvez utiliser des commandes telles que `sed`, `awk`, `grep` et même des boucles et des conditions pour filtrer et manipuler des chaînes de caractères.

Il est également important de noter que Bash dispose d'un support natif pour les tableaux de chaînes de caractères, ce qui facilite la manipulation de sous-chaînes de caractères dans des scripts Bash plus complexes.

## Voir aussi

- [Documentaion Bash sur la commande "cut"](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [Tutoriel sur les expressions régulières en Bash](https://www.tldp.org/LDP/abs/html/regexp.html)
- [Guide complet sur l'utilisation des tableaux en Bash](https://www.linuxjournal.com/content/bash-arrays)