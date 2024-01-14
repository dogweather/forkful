---
title:                "Bash: Utiliser les expressions régulières"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi les expressions régulières sont-elles utiles

Les expressions régulières sont des motifs de recherche qui vous permettent de trouver des correspondances dans du texte. Elles sont très utiles pour les programmeurs car elles permettent d'effectuer des recherches complexes et précises dans des données textuelles.

## Comment utiliser les expressions régulières en Bash

Il existe plusieurs façons d'utiliser les expressions régulières en Bash. La plus simple est d'utiliser l'outil `grep` qui vous permet d'effectuer une recherche dans un fichier ou dans une sortie de commande. Vous pouvez également utiliser les expressions régulières directement dans votre code Bash en utilisant la commande `echo` avec l'option `-E` pour activer les expressions régulières.

Voici un exemple de recherche d'une adresse IP dans un fichier en utilisant `grep` :

```Bash
grep -E "\b([0-9]{1,3}\.){3}[0-9]{1,3}\b" fichier.txt
```

Cela affichera toutes les adresses IP présentes dans le fichier `fichier.txt`. Voici un autre exemple en utilisant `echo` :

```Bash
echo "Mon adresse IP est 192.168.1.1" | grep -E "\b([0-9]{1,3}\.){3}[0-9]{1,3}\b"
```

Cela affichera `192.168.1.1` comme correspondance dans la sortie de la commande `echo`.

## Plongée profonde dans les expressions régulières

Les expressions régulières peuvent sembler intimidantes au premier abord, mais une fois que vous en comprenez les bases, elles peuvent être très puissantes. Voici quelques éléments importants à retenir :

- Les caractères spéciaux doivent être échappés avec un `\` pour être utilisés dans une expression régulière.
- Les classes de caractères, comme `[0-9]` pour les chiffres, permettent de rechercher une plage de caractères.
- Les quantificateurs, comme `{1,3}` pour indiquer un minimum et un maximum de répétitions, permettent de rechercher des motifs répétitifs.
- Les groupes entre parenthèses, comme `([a-z]+)` pour les lettres minuscules, permettent de récupérer des parties spécifiques d'une correspondance.

Pour une liste complète des caractères spéciaux et des règles de syntaxe pour les expressions régulières en Bash, vous pouvez consulter la page de manuel correspondante en utilisant la commande `man grep` ou en recherchant des tutoriels en ligne.

## Voir aussi

- [Guide des expressions régulières en Bash](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Pattern-Matching)
- [Expressions régulières sur Wikipedia](https://fr.wikipedia.org/wiki/Expression_r%C3%A9guli%C3%A8
re)
- [Tutoriels sur les expressions régulières en Bash](https://www.linuxtricks.fr/wiki/expressions-regulieres-decoupe-de-strings-en-bash)