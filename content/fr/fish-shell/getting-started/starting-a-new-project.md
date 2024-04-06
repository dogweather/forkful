---
date: 2024-01-20 18:03:26.347536-07:00
description: "How to - Comment faire : Historiquement, la gestion de nouveaux projets\
  \ \xE9tait manuelle, mais des outils comme `git` et des shells comme `Fish` ont\u2026"
lastmod: '2024-04-05T22:51:12.185422-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, la gestion de nouveaux projets \xE9tait manuelle, mais des\
  \ outils comme `git` et des shells comme `Fish` ont simplifi\xE9 ce processus."
title: Lancement d'un nouveau projet
weight: 1
---

## How to - Comment faire :
```Fish Shell
# Créer un nouveau répertoire pour le projet
mkdir mon_nouveau_projet

# Entrer dans le répertoire
cd mon_nouveau_projet

# Initialiser un dépôt git
git init

# Créer un fichier README basique
echo "# Mon Nouveau Projet" >> README.md

# Afficher la structure du projet
tree .
```
``` 
.
├── .git
└── README.md
```

## Deep Dive - Plongée en profondeur
Historiquement, la gestion de nouveaux projets était manuelle, mais des outils comme `git` et des shells comme `Fish` ont simplifié ce processus. Des alternatives existent : `zsh`, `bash`, etc. Fish est prisé pour sa simplicité et sa syntaxe conviviale. Côté implémentation, Fish utilise des fonctions, des événements et des opérateurs propres à lui pour gérer l'environnement de développement.

## See Also - Voir Aussi
- Documentation Fish Shell : [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutoriel Git : [https://git-scm.com/docs/gittutorial](https://git-scm.com/docs/gittutorial)
