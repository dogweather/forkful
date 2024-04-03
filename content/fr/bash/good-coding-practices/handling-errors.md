---
date: 2024-01-26 00:49:29.212956-07:00
description: "G\xE9rer les erreurs dans les scripts Bash, c'est pr\xE9voir o\xF9 les\
  \ choses pourraient mal tourner et y faire face avec \xE9l\xE9gance. Pourquoi ?\
  \ Et bien, cela assure\u2026"
lastmod: '2024-03-13T22:44:58.005287-06:00'
model: gpt-4-1106-preview
summary: "G\xE9rer les erreurs dans les scripts Bash, c'est pr\xE9voir o\xF9 les choses\
  \ pourraient mal tourner et y faire face avec \xE9l\xE9gance."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
```Bash
#!/bin/bash

# Rediriger stderr vers un fichier
grep "quelque chose" fichier.txt 2> erreurs.log

# Gestion des erreurs avec les codes de sortie
if ! grep "quelque chose" fichier.txt; then
    echo "Oups, quelque chose s'est mal passé lors de la recherche de 'quelque chose'."
    exit 1
fi

# Utiliser un piège pour nettoyer avant de quitter sur une erreur
nettoyage() {
  echo "Nettoyage des fichiers temporaires..."
  rm temp_*
}

trap nettoyage ERR

# erreur intentionnelle : le fichier n'existe pas
cat temp_fichier.txt
```

Exemple de sortie lorsqu'une erreur se produit :

```
Nettoyage des fichiers temporaires...
cat: temp_fichier.txt: Aucun fichier ou dossier de ce type
```

## Approfondissement
Le traitement des erreurs dans les scripts Bash remonte aux origines du shell Unix, où des scripts robustes et fiables étaient (et sont toujours) essentiels pour l'administration système et l'automatisation. Traditionnellement, les erreurs dans Bash sont gérées en vérifiant le code de sortie d'une commande, qui, par convention, renvoie 0 en cas de succès et une valeur non nulle en cas d'échec.

Bash a introduit la commande `trap` comme intégrée, permettant aux utilisateurs de spécifier les commandes à exécuter sur divers signaux ou à la sortie du script. Cela est utile pour les tâches de nettoyage ou comme mécanisme de gestion des erreurs en dernier recours.

Il y a aussi la commande `set`, qui peut changer le comportement de Bash en cas d'erreurs. Par exemple, `set -e` fera quitter immédiatement un script si une commande se termine avec un statut non nul, une façon d'échouer rapidement et d'éviter les erreurs en cascade.

Les alternatives à la gestion des erreurs intégrée dans Bash incluent la vérification explicite de l'existence de fichiers, l'utilisation de la substitution de commande, ou même la rédaction de vos propres fonctions pour gérer les erreurs de manière plus granulaire.

Bien que la gestion rigoureuse des erreurs puisse parfois sembler excessive pour des petits scripts, c'est une pratique qui peut économiser beaucoup de temps de débogage et prévenir des comportements inattendus tant pour vous que pour les utilisateurs.

## Voir aussi
- Manuel Bash sur les paramètres du shell : https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- Guide de scriptage Bash avancé sur la gestion des erreurs : https://www.tldp.org/LDP/abs/html/exit-status.html
- Un guide approfondi sur `trap` : https://mywiki.wooledge.org/SignalTrap

Rappelez-vous, écrire des scripts est une forme d'art, et la façon dont vous gérez les pépins et les trébuchements peut rendre votre chef-d'œuvre plus résilient. Bon scripting !
