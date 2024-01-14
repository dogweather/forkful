---
title:                "Bash: Mettre en majuscule une chaîne de caractères"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture du code peut sembler intimidante pour certains, mais il existe en fait des moyens simples de rendre le processus plus facile et plus efficace. Un de ces moyens est le fait de mettre en majuscule une chaîne de caractères. Dans cet article, nous allons voir pourquoi et comment effectuer cette action dans Bash. 

## Comment faire

La capitale d'une chaîne de caractères est essentiellement le fait de la convertir en lettres majuscules. Dans Bash, il existe plusieurs façons de le faire. Tout d'abord, nous pouvons utiliser la commande "tr" avec l'option "-s" (squeeze) pour supprimer les espaces inutiles et "-u" (upper) pour mettre en majuscule. Par exemple:

```
Bash code block:
echo "bonjour tout le monde" | tr -s [:space:] | tr '[:lower:]' '[:upper:]'
```

Cela produira la sortie suivante: "BONJOUR TOUT LE MONDE". Nous pouvons également utiliser la commande "awk" et la fonction "toupper" pour mettre en majuscule une chaîne de caractères. Par exemple:

```
Bash code block:
echo "ceci est un exemple" | awk '{ print toupper($0) }'
```

Cela produira la sortie suivante: "CECI EST UN EXEMPLE". Enfin, nous pouvons également utiliser la commande "sed" avec l'option "y" pour effectuer une substitution de caractères en utilisant la table ASCII. Par exemple:

```
Bash code block:
echo "cOmPter les cArActères" | sed 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'
```

Cela produira la sortie suivante: "COMPTER LES CARACTERES". 

## Plongée Profonde

Maintenant que nous avons vu différentes façons de mettre en majuscule une chaîne de caractères, il est important de comprendre comment cela fonctionne en profondeur. En Bash, les chaînes de caractères sont considérées comme des tableaux de caractères individuels. Ainsi, pour mettre en majuscule une chaîne, nous devons itérer sur tous les caractères et remplacer les lettres minuscules par leur équivalent majuscule en utilisant la table ASCII. C'est ce que les commandes "tr", "awk" et "sed" font en interne. 

## Voir aussi

Maintenant que vous avez appris comment mettre en majuscule une chaîne de caractères en Bash, vous pouvez également consulter les ressources suivantes pour améliorer vos compétences en programmation :

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/)
- [Guide de démarrage rapide de Bash](https://linuxconfig.org/bash-scripting-tutorial)
- [Cours interactif sur Bash](https://www.learnshell.org/)