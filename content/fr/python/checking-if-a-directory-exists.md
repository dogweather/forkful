---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Python: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le vérifier?

"Vérifier si un répertoire existe" fait référence à une vérification programmée pour voir s'il existe un répertoire avec un certain nom dans un système de fichiers. Les programmeurs font cela pour s'assurer qu'ils n'utilisent pas un répertoire qui n'existe pas ou pour exécuter une certaine logique en fonction de l'existence ou non d'un répertoire.

# Comment:

```python
import os
dir_name = "/chemin/vers/le/répertoire"
if os.path.exists(dir_name):
    print("Le répertoire existe!")
else:
    print("Le répertoire n'existe pas!")

```

Sortie:
```bash
Le répertoire existe! 
```

# Plongée en profondeur:

## Contexte historique:
La vérification de l'existence d'un répertoire est devenue plus importante avec l'avènement des systèmes de fichiers en réseau et des systèmes d'exploitation multi-utilisateurs. Elle permet de s'assurer que chaque utilisateur a accès à un répertoire dédié pour stocker ses fichiers.

## Alternatives:
Au lieu de vérifier si un répertoire existe, certains programmeurs peuvent choisir de le créer à la volée s'il n'existe pas. Cela peut parfois être plus pratique, surtout lorsqu'il s'agit de créer un nouveau répertoire pour stocker des fichiers ou des données.

## Détails d'implémentation:
Pour vérifier si un répertoire existe, le module os de Python fournit la méthode `os.path.exists ()` qui renvoie `True` si le répertoire existe et `False` s'il n'existe pas.

# Voir aussi:
- Documentation officielle de la méthode os.path.exists (): https://docs.python.org/fr/3/library/os.path.html#os.path.exists
- Tutoriel sur la gestion des fichiers et des répertoires en Python: https://www.digitalocean.com/community/tutorials/how-to-handle-file-uploads-in-python-3