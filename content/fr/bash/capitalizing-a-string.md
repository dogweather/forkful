---
title:                "Bash: Mise en majuscule d'une chaîne de caractères"
simple_title:         "Mise en majuscule d'une chaîne de caractères"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous programmez en Bash, il peut être utile de savoir comment capitaliser une chaîne de caractères. Cela peut être particulièrement pratique lorsque vous travaillez avec des noms de fichiers ou des chaînes de caractères qui doivent être formatés correctement.

# Comment Faire

Voici un exemple simple de code Bash pour capitaliser une chaîne de caractères :

```
variable="bonjour les amis"
echo "${variable^}"
```

Le résultat de cette commande sera "Bonjour les amis", où la première lettre de chaque mot est en majuscule. Vous pouvez également utiliser "${variable^^}" pour mettre en majuscule toutes les lettres de la chaîne. Voici un exemple de sortie pour cette commande :

```
variable="bonjour les amis"
echo "${variable^^}"
```

Ce qui donnera comme résultat "BONJOUR LES AMIS".

# En Profondeur

Il est important de noter que la façon dont vous capitaliser une chaîne dépendra de la configuration de la variable locale. Si vous utilisez le paramètre "set -x", cette commande mettra temporairement votre script en mode de débogage où toutes les commandes sont imprimées sur la sortie standard, ce qui peut être utile pour comprendre votre code.

Il existe également d'autres façons de capitaliser une chaîne en Bash, comme en utilisant la commande "tr", qui peut convertir des caractères d'une chaîne en d'autres caractères, en utilisant des "sed" ou "awk" ou encore en utilisant des expressions régulières.

# Voir Aussi

- [Apprendre le Shell - Expansions](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_04.html)
- [Apprendre le Shell - Sous chaînes](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_02.html)
- [Guide du Développeur GNU Bash - Manipulation de Variables](https://www.gnu.org/software/bash/manual/html_node/Manipulating-Variables.html)