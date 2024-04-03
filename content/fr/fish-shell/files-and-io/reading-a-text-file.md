---
date: 2024-01-20 17:54:14.683057-07:00
description: "Lire un fichier texte, c'est extraire son contenu pour l'utiliser. Les\
  \ programmeurs le font pour manipuler des donn\xE9es, configurer des syst\xE8mes,\
  \ ou\u2026"
lastmod: '2024-03-13T22:44:58.343141-06:00'
model: gpt-4-1106-preview
summary: Lire un fichier texte, c'est extraire son contenu pour l'utiliser.
title: Lecture d'un fichier texte
weight: 22
---

## Quoi & Pourquoi ?
Lire un fichier texte, c'est extraire son contenu pour l'utiliser. Les programmeurs le font pour manipuler des données, configurer des systèmes, ou automatiser des tâches.

## Comment faire :
```Fish Shell
# Pour afficher le contenu d'un fichier texte
cat mon_fichier.txt

# Pour lire ligne par ligne
while read -la line
    echo $line
end < mon_fichier.txt

# Exemple de sortie
La première ligne de texte
La deuxième ligne de texte
```
## Exploration en profondeur
Historiquement, lire des fichiers est fondamental en programmation. En Unix, tout est fichier, d'où l'importance de maitriser cette opération. Sous Fish, `cat` est simple pour afficher des fichiers, mais des commandes comme `read` sont utiles pour un traitement plus fin, lire ligne par ligne par exemple. Comparé à d'autres shells, Fish se distingue par des syntaxes plus lisibles. Il existe aussi des utilitaires comme `awk` ou `sed` pour des manipulations avancées ; toutefois, Fish seul peut suffire pour des opérations de base.

## Voir aussi
- Documentation officielle de Fish : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Guide Fish pour travailler avec des fichiers : [https://fishshell.com/docs/current/tutorial.html#tut_files](https://fishshell.com/docs/current/tutorial.html#tut_files)
- Pour approfondir `awk` : [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html)
- Pour approfondir `sed` : [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
