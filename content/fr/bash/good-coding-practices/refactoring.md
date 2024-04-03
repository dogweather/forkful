---
date: 2024-01-26 01:16:29.756507-07:00
description: "Le refactoring est le processus de restructuration du code informatique\
  \ existant sans en changer le comportement externe. C'est une pratique essentielle\u2026"
lastmod: '2024-03-13T22:44:58.006409-06:00'
model: gpt-4-0125-preview
summary: Le refactoring est le processus de restructuration du code informatique existant
  sans en changer le comportement externe.
title: Refactoring
weight: 19
---

## Comment faire :
Considérons un script Bash simple qui nécessite un peu de refactoring. Il est maladroit, avec du code répété et il est difficile à suivre :

```Bash
#!/bin/bash
echo "Entrez un nom de fichier :"
read filename
if [ -f "$filename" ]; then
    echo "Le fichier existe."
    count=$(grep -c "foo" "$filename")
    echo "Le mot foo apparaît $count fois."
else
    echo "Le fichier n'existe pas."
fi
```

Le refactoring pour plus de clarté et de réutilisabilité pourrait impliquer l'introduction de fonctions et une gestion des erreurs plus élégante :

```Bash
#!/bin/bash

function fichier_existe() {
    [[ -f "$1" ]]
}

function compter_occurrences() {
    grep -c "$1" "$2"
}

function principal() {
    local nom_fichier mot count
    echo "Entrez un nom de fichier :"
    read -r nom_fichier
    echo "Entrez le mot à rechercher :"
    read -r mot

    if fichier_existe "$nom_fichier"; then
        count=$(compter_occurrences "$mot" "$nom_fichier")
        echo "Le mot $mot apparaît $count fois."
    else
        echo "Le fichier n'existe pas." >&2
        exit 1
    fi
}

principal "$@"
```

La version refactorisée utilise des fonctions pour améliorer la lisibilité et permet une réutilisation potentielle.

## Plongée profonde :
Le refactoring n'est pas un concept qui a originalement commencé avec Bash ou même les langages de programmation de haut niveau ; il est aussi vieux que la programmation elle-même. Le terme a été formalisé dans le livre "Refactoring: Improving the Design of Existing Code" par Martin Fowler en 1999, se concentrant principalement sur les langages orientés objet.

Dans le contexte des scripts Bash, le refactoring signifie souvent décomposer de longs scripts en fonctions, réduire la répétition avec des boucles ou des conditionnelles, et éviter les pièges courants comme ne pas gérer les espaces dans les noms de fichiers. Les alternatives à Bash pour des scripts devenus trop complexes incluent Python ou Perl, qui offrent de meilleures structures de données et une gestion des erreurs pour les tâches complexes.

Le refactoring spécifique à Bash est davantage une question d'adhésion aux meilleures pratiques, telles que mettre les variables entre guillemets, utiliser `[[ ]]` pour les tests à la place de `[ ]`, et préférer `printf` à `echo` pour une sortie robuste. Les détails de mise en œuvre tournent souvent autour de l'adhésion aux guides de style et à l'utilisation d'outils comme `shellcheck` pour une analyse statique afin de détecter les erreurs courantes.

## Voir aussi :
- [Guide de style Shell de Google](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, un outil d'analyse statique pour les scripts shell](https://www.shellcheck.net/)
- [L'Art de la Ligne de Commande](https://github.com/jlevy/the-art-of-command-line)
