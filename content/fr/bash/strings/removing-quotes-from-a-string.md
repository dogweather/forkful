---
date: 2024-01-26 03:37:53.629879-07:00
description: "Comment faire : Bash offre plusieurs fa\xE7ons de retirer les guillemets\
  \ des cha\xEEnes. Voici quelques exemples rapides ."
lastmod: '2024-03-13T22:44:57.975896-06:00'
model: gpt-4-0125-preview
summary: "Bash offre plusieurs fa\xE7ons de retirer les guillemets des cha\xEEnes."
title: "Retirer les guillemets d'une cha\xEEne"
weight: 9
---

## Comment faire :
Bash offre plusieurs façons de retirer les guillemets des chaînes. Voici quelques exemples rapides :

```Bash
#!/bin/bash

# Utiliser la substitution de variable pour retirer à la fois les guillemets simples et doubles
STRING="\"Bonjour, Monde !\""
echo ${STRING//\"}

# Utiliser `tr` pour supprimer les guillemets
STRING="'Bonjour, Monde !'"
echo $STRING | tr -d "\'"

# Utiliser `sed` pour supprimer les guillemets
STRING="\"Bonjour, Monde !\""
echo $STRING | sed 's/"//g'
```

Exemple de sortie :

```
Bonjour, Monde !
Bonjour, Monde !
Bonjour, Monde !
```

## Exploration approfondie
Il fut un temps, les commandes Unix comme `tr` et `sed` étaient les principaux outils pour le traitement de texte. Ils sont encore utilisés aujourd'hui pour leur flexibilité et leur puissance dans la gestion des transformations de texte, comme la suppression de guillemets. Ils sont un élément essentiel dans la boîte à outils de tout scripteur de shell.

Bash lui-même a depuis évolué et la substitution de variable ajoute une couche de simplicité pour les manipulations de chaînes à petite échelle. Cela vous évite de passer par des binaires externes, rendant vos scripts un peu plus efficaces.

Alors que `tr` est excellent pour supprimer des caractères, il ne gère pas des motifs plus complexes. `Sed`, d'autre part, utilise des expressions régulières, donc c'est parfois exagéré et pourrait être plus lent pour des opérations simples.

Le choix entre ces méthodes dépend de votre cas spécifique. Si vous avez besoin de retirer une variété de guillemets et que vous êtes déjà dans le contexte d'un script Bash, utiliser la substitution de variable est une évidence pour sa simplicité. Mais si vous transformez des flux de texte ou des données multi-lignes, `tr` et `sed` sont vos amis privilégiés.

## Voir également :
- Le manuel GNU Bash, en particulier les sections sur l'expansion des paramètres et l'expansion des paramètres du shell : https://www.gnu.org/software/bash/manual/
- Le manuel de la commande `tr` : https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- L'aperçu de l'éditeur de flux `sed` : https://www.gnu.org/software/sed/manual/sed.html
