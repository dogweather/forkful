---
date: 2024-01-20 17:55:07.735854-07:00
description: ''
lastmod: '2024-04-05T22:51:11.380497-06:00'
model: gpt-4-1106-preview
summary: ''
title: Lecture d'un fichier texte
weight: 22
---

## Comment faire :


### Lire tout le fichier
```Python
with open('exemple.txt', 'r') as fichier:
    contenu = fichier.read()
print(contenu)
```

Sortie potentielle :

```
Bonjour, voici le contenu de votre fichier texte !
```

### Lire ligne par ligne
```Python
with open('exemple.txt', 'r') as fichier:
    for ligne in fichier:
        print(ligne.strip())
```

Sortie ligne par ligne :

```
Bonjour,
voici le contenu
de votre fichier texte !
```

### Utilisation de `readlines()`
```Python
with open('exemple.txt', 'r') as fichier:
    lignes = fichier.readlines()
lignes = [ligne.strip() for ligne in lignes]
print(lignes)
```

Sortie en liste :

```
['Bonjour,', 'voici le contenu', 'de votre fichier texte !']
```

## Exploration détaillée
À l'ère des débuts de l'informatique, lire un fichier était plus compliqué, impliquant souvent de composer avec des interfaces système bas niveau. Python a simplifié cela en introduisant des abstractions pratiques.

### Alternatives
- `open()` est le standard, mais des modules comme `os` et `io` offrent des fonctionnalités avancées ou différentes manières de manipuler les fichiers.

### Détails d'implémentation
- `open()` peut prendre différents modes, comme 'r' (lecture seule), 'w' (écriture, efface tout contenu existant) ou 'a' (ajout à la fin du fichier).
- Utiliser `with` pour ouvrir un fichier assure qu'il sera fermé correctement, même en cas d'erreur. C'est toujours une meilleure pratique.
- Python utilise la codification des fichiers, par défaut UTF-8, mais il faut l'ajuster si le fichier a une codification différente.
- Lire de gros fichiers peut être optimisé en les traitant ligne par ligne ou par morceaux.

## Voir aussi
- La documentation Python sur les fichiers et les modes d'ouverture : https://docs.python.org/fr/3/library/functions.html#open
- Le tutoriel Python sur la lecture et l'écriture des fichiers : https://docs.python.org/fr/3/tutorial/inputoutput.html#reading-and-writing-files
- Un guide Stack Overflow pour gérer l’encodage en Python : https://stackoverflow.com/questions/17912307/u-ufeff-in-python-string
