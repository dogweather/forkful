---
date: 2024-01-20 17:57:51.581798-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:58.306646-06:00'
model: gpt-4-1106-preview
summary: .
title: Recherche et remplacement de texte
weight: 10
---

## Comment faire :
```Fish Shell
# Recherche 'fish' et remplace par 'shark' dans une chaîne
echo "I love to fish in the fishy waters" | string replace "fish" "shark"
# Résultat :
I love to shark in the sharky waters

# Recherche et remplacement globaux avec l'option -a/--all
echo "fish fish fish!" | string replace -a "fish" "salmon"
# Résultat :
salmon salmon salmon!

# Utilisation d'une expression régulière (regex) pour remplacer des chiffres par des mots
echo "My top 3 favorites are 1, 2, and 3" | string replace -r '(\d+)' 'number \1'
# Résultat :
My top 3 favorites are number 1, number 2, and number 3
```

## Exploration plus profonde
Historiquement, la recherche et remplacement étaient des tâches réalisées manuellement. Avec l'arrivée des éditeurs de texte et des outils comme `sed` dans Unix, c'est devenu une formalité. Fish Shell a modernisé cela avec la fonction ‘string replace’ qui est plus lisible et facile à utiliser comparé à l’utilisation de `sed`. En plus d'être intégré dans Fish, il supporte directement les regex, ce qui augmente sa puissance.

Concernant les alternatives, `sed` reste une option viable, surtout pour les scripts shell traditionnels. Perl et Python offrent aussi d'excellentes librairies pour les manipulations de texte avancées. L'implémentation dans Fish est conçue pour la simplicité et l'intuitivité, faisant souvent de `string` l'outil idéal pour les scripts rapides et interactif.

## Voir aussi
- Documentation officielle de Fish sur `string`: https://fishshell.com/docs/current/cmds/string.html
- Tutoriel sur les expressions régulières (regex): https://www.regular-expressions.info/
- Guide sed & awk pour des tâches de recherche/remplacement complexes: https://www.gnu.org/software/sed/manual/sed.html
