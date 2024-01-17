---
title:                "Lancer un nouveau projet"
html_title:           "Haskell: Lancer un nouveau projet"
simple_title:         "Lancer un nouveau projet"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lancer un nouveau projet en Haskell, c'est commencer à écrire du code et à concevoir un programme à partir de zéro. Les programmeurs le font pour résoudre un problème spécifique ou pour créer une nouvelle application.

## Comment faire:

```Haskell
-- Création d'un nouveau projet

stack new mon_projet

-- Compilation et exécution du code

cd mon_projet
stack build
stack exec mon_projet

-- Ajouter des dépendances

-- Exemple avec le package "Data.List"
-- Dans le fichier "stack.yaml" ajouter:

Extra-Dep-Templates:
- package: containers
  extra-dep: false
  buildable: false

-- Dans le fichier ".cabal" ajouter les lignes suivantes:

build-depends:
    base,
    containers,
    -- autres dépendances

-- Importer le package dans le fichier source

import Data.List
```

## Plongée en profondeur:

### Contexte historique:
Le langage Haskell a été créé dans les années 1980 par des chercheurs en informatique pour combler un manque de langages fonctionnels. Il est devenu populaire dans les années 1990 grâce à son système de types fort et son support pour la programmation fonctionnelle pure.

### Alternatives:
Il existe d'autres langages de programmation pour démarrer un nouveau projet, tels que Java, Python ou C++. Chacun a ses propres avantages et inconvénients.

### Détails de mise en œuvre:
Pour démarrer un nouveau projet en Haskell, il est recommandé d'utiliser un outil de gestion de paquets tel que Stack ou Cabal. Ces outils facilitent l'installation et la mise à jour des dépendances, ainsi que la compilation et l'exécution du code.

## Voir aussi:

- Site officiel de Haskell: https://www.haskell.org/
- Tutoriels de Haskell: https://wiki.haskell.org/Tutorials
- Outils de gestion de paquets: https://www.haskell.org/downloads#stack, https://www.haskell.org/cabal/